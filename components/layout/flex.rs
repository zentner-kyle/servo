/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

//! Layout for elements with a CSS `display` property of `flex`.

#![deny(unsafe_code)]

use app_units::Au;
use block::BlockFlow;
use context::LayoutContext;
use display_list_builder::FlexFlowDisplayListBuilding;
use euclid::{Point2D, Rect};
use floats::FloatKind;
use flow;
use flow::INLINE_POSITION_IS_STATIC;
use flow::IS_ABSOLUTELY_POSITIONED;
use flow::ImmutableFlowUtils;
use flow::mut_base;
use flow::{Flow, FlowClass, OpaqueFlow};
use flow::{HAS_LEFT_FLOATED_DESCENDANTS, HAS_RIGHT_FLOATED_DESCENDANTS};
use fragment::{Fragment, FragmentBorderBoxIterator};
use gfx::display_list::DisplayList;
use incremental::{REFLOW, REFLOW_OUT_OF_FLOW};
use layout_debug;
use style::computed_values::{box_sizing, flex_basis, flex_direction, float};

use model::MaybeAuto;
use model::{IntrinsicISizes};
use std::cmp::max;
use std::sync::Arc;
use style::computed_values::{flex_direction, float};
use style::properties::ComputedValues;
use style::properties::style_structs;
use style::values::computed::LengthOrPercentageOrAuto;
use util::geometry::Au;
use util::logical_geometry::{LogicalSize, WritingMode};
use util::opts;

// A mode describes which logical axis a flex axis is parallel with.
// The logical axises are inline and block, the flex axises are main and cross.
// When the flex container has flex-direction: column or flex-direction: column-reverse, the main axis
// should be block. Otherwise, it should be inline.
#[derive(Clone, Copy, Debug)]
enum Mode {
    Inline,
    Block
}

impl Mode {
    fn is_vertical(&self, writing_mode: WritingMode) -> bool {
        match self {
            Inline => writing_mode.is_vertical(),
            Block => !writing_mode.is_vertical()
        }
    }
}

struct FlexItem {
    base_size: Au,
    hypothetical_main_size: Au,
}

/// A block with the CSS `display` property equal to `flex`.
#[derive(Debug)]
pub struct FlexFlow {
    /// Data common to all block flows.
    block_flow: BlockFlow,
    /// The logical axis which the main axis will be parallel with.
    /// The cross axis will be parallel with the opposite logical axis.
    main_mode: Mode,
    items: Vec<FlexItem>
}

// Returns the style struct for the properties of a flex container's fragment.
fn flex_style(style: &ComputedValues) -> &style_structs::Flex {
    style.get_flex()
}

// Returns the style struct for the properties of a flex items's fragment.
fn flex_item_style(style: &ComputedValues) -> &style_structs::Flex {
    style.get_flex()
}

fn flex_item_compute_used_flex_basis(flow: &Flow, item_content_main_size: MaybeAuto,
                                     container_intrinsic_size: MaybeAuto,
                                     container_main_size: MaybeAuto) -> Au {
    let content_size = item_content_main_size.specified_or_default(Au(0));
    let auto_size = container_intrinsic_size.specified_or_default(content_size);
    match flex_item_style(&flow.as_immutable_block().fragment.style).flex_basis {
        flex_basis::T::Width(width) => match width {
            LengthOrPercentageOrAuto::Auto => auto_size,
            LengthOrPercentageOrAuto::Length(len) => len,
            LengthOrPercentageOrAuto::Percentage(per) =>
                if let MaybeAuto::Specified(main_size) = container_main_size {
                    main_size.scale_by(per)
                } else {
                    content_size
                }
        },
        flex_basis::T::Content => {
            content_size
        }
    }
}

fn compute_flex_item_border_padding(flow: &Flow, containing_size: LogicalSize<Au>) -> LogicalMargin {
    let fragment = &flow.as_immutable_block().fragment;
    let padding = get_flex_item_padding(fragment.style(), containing_size);
    let border = fragment.style().logical_border_width();
    border + padding
}

fn compute_definite(length: LengthOrPercentageOrAuto, resolving_length: Au) -> MaybeAuto {
    MaybeAuto::from_style(length, resolving_length)
}

fn compute_definite_or_zero(length: LengthOrPercentageOrAuto, resolving_length: Au) -> Au {
    compute_definite(length, resolving_length).specified_or_zero()
}

// This function is needed because, according to Section 4.2, percentage values in the top and
// bottom padding of the flex items are resolved against the flex container's block size.
//
// This is the second half of that text:
//
//     Percentage margins and paddings on flex items are always resolved against their respective
//     dimensions; unlike blocks, they do not always resolve against the inline dimension of their
//     containing block.
//
// If Tab Atkins Jr. "re-litigates" this behavior away, this function can disappear.
fn compute_flex_item_padding(style: &ComputedValues, containing_size: LogicalSize)
    -> LogicalMargin<Au> {
    let padding_style = style.get_padding();
    LogicalMargin::from_physical(style.writing_mode, SideOffsets2D::new(
        compute_definite_or_zero(padding_style.padding_top, containing_size.block),
        compute_definite_or_zero(padding_style.padding_right, containing_size.inline),
        compute_definite_or_zero(padding_style.padding_bottom, containing_size.block),
        compute_definite_or_zero(padding_style.padding_left, containing_size.inline)))
}

// According to Section 4.2, flex item's do no participate in margin collapse, we can meaningfully
// calculate the margin of each flex item separately.
// Note that the rules here are different from block layout, like in the previous function.
fn compute_flex_item_margin(style: &ComputedValues, containing_size: LogicalSize)
    -> LogicalMargin<Au> {
    let margin = style.logical_margin();
    LogicalMargin::new(style.writing_mode,
        compute_definite_or_zero(margin.block_start, containing_size.block),
        compute_definite_or_zero(margin.inline_end, containing_size.inline),
        compute_definite_or_zero(margin.block_end, containing_size.block),
        compute_definite_or_zero(margin.inline_start, containing_size.inline))
}

fn get_box_sizing_adjustment(mode: Mode, flow: &Flow, containing_size: LogicalSize<Au>) -> Au {
    let fragment = &flow.as_immutable_block().fragment;
    match fragment.style.get_box().box_sizing {
        box_sizing::T::border_box =>
            get_axis_from_size(mode, get_flex_item_border_padding(flow, containing_size)),
        box_sizing::T::content_box => Au(0)
    }
}

fn get_axis_from_size(mode: Mode, size: LogicalSize<Au>) -> Au {
    match mode {
        Mode::Inline => size.inline,
        Mode::Block  => size.block
    }
}

fn get_item_definite_content_size(mode: Mode, flow: &Flow, containing_size: LogicalSize<Au>) -> MaybeAuto {
    let fragment = &flow.as_immutable_block().fragment;
    match mode {
        Mode::Inline =>
            MaybeAuto::from_style(fragment.style.content_inline_size(), containing_size.inline),
        Mode::Block =>
            MaybeAuto::from_style(fragment.style.content_block_size(), containing_size.block)
    }
}

fn get_aspect_ratio (flow: &Flow) -> Option<CSSFloat> {
    // TODO(zentner): Use flex items' intrinsic aspect ratios.
    None
}

fn get_definite_cross_size(cross_mode: Mode, flow: &Flow, containing_size: LogicalSize<Au>) -> Option<Au> {
    if let MaybeAuto::Specified(content_size) = get_item_definite_content_size(cross_mode, flow,
                                                                               containing_size) {
        let fragment = &flow.as_immutable_block().fragment;
        Some(content_size + get_axis_from_size(cross_mode, fragment.border_padding()) +
             get_axis_from_size(cross_mode, fragment.margin()))
    } else {
        None
    }
}

fn get_used_flex_basis(mode: Mode, fragment: &Fragment) {
    let basis = flex_item_style(fragment.style).flex_basis;
    //{
        //basis = flex_item_style(fragment.style).flex_basis;
    //}
    let main_size = match mode {
        Mode::Inline => fragment.style.content_inline_size(),
        Mode::Block => fragment.style.content_block_size(),
    };
    match (basis, main_size) {
        (flex_basis::T::Content, _) => flex_basis::T::Content,
        (flex_basis::T::Width(Auto), computed::LengthOrPercentageOrAuto::Auto) => flex_basis::T::Content,
        (basis, _) => basis
    }
}

fn get_frag(flow: &Flow) -> &Fragment {
    &flow.as_immutable_block().fragment
}


fn flex_item_inline_sizes(main_mode: Mode, flow: &Flow) -> IntrinsicISizes {
    let _scope = layout_debug_scope!("flex::flex_item_inline_sizes");
    let intrinsic_pref_size;
    let intrinsic_min_size;
    {
        let base = flow::base(flow);
        intrinsic_min_size = base.intrinsic_inline_sizes.minimum_inline_size;
        intrinsic_pref_size = base.intrinsic_inline_sizes.preferred_inline_size;
    }

    match main_mode {
        Mode::Inline => {
            let adjustment_for_box_sizing;
            let content_inline_size;
            {
                let fragment = &flow.as_immutable_block().fragment;
                adjustment_for_box_sizing = match fragment.style.get_box().box_sizing {
                    box_sizing::T::border_box => fragment.border_padding.block_start_end(),
                    box_sizing::T::content_box => Au(0),
                };
                content_inline_size = fragment.style.content_inline_size();
            }
            let min_basis = flex_item_compute_used_flex_basis(
                flow,
                MaybeAuto::Specified(intrinsic_min_size - adjustment_for_box_sizing),
                MaybeAuto::from_style(content_inline_size, Au(0)),
                MaybeAuto::Auto);
            let pref_basis = flex_item_compute_used_flex_basis(
                flow,
                MaybeAuto::Specified(intrinsic_pref_size - adjustment_for_box_sizing),
                MaybeAuto::from_style(content_inline_size, Au(0)),
                MaybeAuto::Auto);
            // TODO(zentner): Is calculating two different bases here correct?
            // Gecko *appears* to do so, but it's logic for flex-basis is spread across several
            // functions, making it relatively hard to understand / check for correctness.
            IntrinsicISizes {
                minimum_inline_size: min_basis + adjustment_for_box_sizing,
                preferred_inline_size: pref_basis + adjustment_for_box_sizing,
            }
        },
        Mode::Block => {
            let base = flow::base(flow);
            IntrinsicISizes {
                minimum_inline_size: base.intrinsic_inline_sizes.minimum_inline_size,
                preferred_inline_size: base.intrinsic_inline_sizes.preferred_inline_size,
            }
        }
    }
}

impl FlexItem {
    fn zero() -> FlexItem {
        FlexItem {
            base_size: Au(0),
            hypothetical_main_size: Au(0)
        }
    }

}

// TODO(zentner): This function should use flex-basis.
impl FlexFlow {
    pub fn from_fragment(fragment: Fragment,
                         flotation: Option<FloatKind>)
                         -> FlexFlow {

        let main_mode = match flex_style(&fragment.style).flex_direction {
            flex_direction::T::row_reverse    => Mode::Inline,
            flex_direction::T::row            => Mode::Inline,
            flex_direction::T::column_reverse => Mode::Block,
            flex_direction::T::column         => Mode::Block
        };

        let mut this = FlexFlow {
            block_flow: BlockFlow::from_fragment(fragment, flotation),
            main_mode: main_mode,
            items: Vec::new()
        };

        let child_count = ImmutableFlowUtils::child_count(self as &Flow);

        this.items.reserve(child_count);
        for _ in (0..child_count) {
            this.items.push(FlexItem::zero());
        }

        this
    }

    // This function implements Flexbox 9.2.3
    fn flex_item_compute_base_size(&self, flow: &Flow, containing_size:
                                   LogicalSize<Au>) -> MaybeAuto {
        // The result of Flexbox 9.2.2 is stored in containing_size.

        let main_mode = self.main_mode;
        let self_wm = get_frag(self).style.writing_mode;

        let basis = get_used_flex_basis(main_mode, get_frag(flow));
        let containing_main_size = get_axis_from_size(main_mode, containing_size);
        let item_wm = get_frag(flow).style.writing_mode;

        // Flexbox 9.2.3.A:
        if let flex_basis::T::Width(width) = basis {
            match (width, containing_main_size) {
                (Length(len), _) => { return len; },
                (Percentage(per), Some(len)) => { return len.scale_by(per); },
                _ => {}
            }
        }

        // Flexbox 9.2.3.B:
        if let (Some(aspect_ratio), flex_basis::T::Content, Some(size)) =
            (get_aspect_ratio(flow), basis, get_definite_cross_size(main_mode.other(), flow,
                                                                    containing_size)) {
            return size;
        }

        // Flexbox 9.2.3.C:
        match (basis, self.main_size_constraint) {
            (flex_basis::T::Content | flex_basis::T::Width(LengthOrPercentageOrAuto::Auto),
             Some(len)) => {
                // FIXME(zentner): I don't think that this is correct.
                // TODO(zentner): Find out what "size the item under that constraint" means.
                return len;
            },
            _ = {}
        }

        // Flexbox 9.2.3.D:
        match (basis, containing_main_size, main_mode.is_vertical(self_wm) == item_wm.is_vertical())  {
            (flex_basis::T::Content | flex_basis::T::Width(LengthOrPercentageOrAuto::Percentage(_)),
            None,
            true) => {
                // Lay out the item using the rules for a box in an orthogonal flow.
                // Return the item's max-content main size.
                return get_axis_from_size(main_mode, layout_orthogonal_flow(flow));
            }
            _ => {}
        }

        // Flexbox 9.2.3.E

    }

    fn compute_flex(&mut self, size: LogicalSize<Au>) {
        // Flexbox Section 9.2 has already been done at this point.
        // The results of that section have been stored in `size`.
        for kid in self.block_flow.base.child_iter() {       
            self.update_fragment_border_padding_margin(kid, size);
        }

        // Flexbox Section 9.3: Determine flex items' base and hypothetical main size.
    }

    // Update a child flow's fragment's border_padding, and margin once we know our own size.
    fn update_fragment_border_padding_margin(&self, flow: &mut Flow, containing_size: LogicalSize<Au>) {
        let border_padding : LogicalMargin<Au>;
        let margin : LogicalMargin<Au>;
        {
            let flow : &Flow = flow;
            border_padding = compute_flex_item_border_padding(flow, containing_size);
            margin = compute_flex_item_margin(&flow.as_immutable_block().fragment.style(), containing_size);
        }
        let mut fragment = flow.as_block().fragment;
        fragment.border_padding = border_padding;
        fragment.margin = margin;
    }


    // Currently, this is the core of BlockFlow::bubble_inline_sizes() with all float logic
    // stripped out, and max replaced with union_nonbreaking_inline.
    fn inline_mode_bubble_inline_sizes(&mut self) {
        let fixed_width = match self.block_flow.fragment.style().get_box().width {
            LengthOrPercentageOrAuto::Length(_) => true,
            _ => false,
        };

        let main_mode = self.main_mode;
        let mut computation = self.block_flow.fragment.compute_intrinsic_inline_sizes();
        if !fixed_width {
            for kid in flow::imm_child_iter(self as &Flow) {
                //self.block_flow.base.imm_child_iter() {
                let is_absolutely_positioned =
                    flow::base(kid).flags.contains(IS_ABSOLUTELY_POSITIONED);
                if !is_absolutely_positioned {
                    computation.union_nonbreaking_inline(&flex_item_inline_sizes(main_mode, kid));
                }
            }
        }
        self.block_flow.base.intrinsic_inline_sizes = computation.finish();
    }

    // Currently, this is the core of BlockFlow::bubble_inline_sizes() with all float logic
    // stripped out.
    fn block_mode_bubble_inline_sizes(&mut self) {
        let fixed_width = match self.block_flow.fragment.style().get_box().width {
            LengthOrPercentageOrAuto::Length(_) => true,
            _ => false,
        };

        let mut computation = self.block_flow.fragment.compute_intrinsic_inline_sizes();
        if !fixed_width {
            for kid in self.block_flow.base.child_iter() {
                let is_absolutely_positioned =
                    flow::base(kid).flags.contains(IS_ABSOLUTELY_POSITIONED);
                let child_base = flow::mut_base(kid);
                if !is_absolutely_positioned {
                    computation.content_intrinsic_sizes.minimum_inline_size =
                        max(computation.content_intrinsic_sizes.minimum_inline_size,
                            child_base.intrinsic_inline_sizes.minimum_inline_size);

                    computation.content_intrinsic_sizes.preferred_inline_size =
                        max(computation.content_intrinsic_sizes.preferred_inline_size,
                            child_base.intrinsic_inline_sizes.preferred_inline_size);
                }
            }
        }
        self.block_flow.base.intrinsic_inline_sizes = computation.finish();
    }

    // TODO(zentner): This function needs to be radically different for multi-line flexbox.
    // Currently, this is the core of BlockFlow::propagate_assigned_inline_size_to_children() with
    // all float and table logic stripped out.
    fn block_mode_assign_inline_sizes(&mut self,
                                      _layout_context: &LayoutContext,
                                      inline_start_content_edge: Au,
                                      inline_end_content_edge: Au,
                                      content_inline_size: Au) {
        let _scope = layout_debug_scope!("flex::block_mode_assign_inline_sizes");
        debug!("block_mode_assign_inline_sizes");

        // Calculate non-auto block size to pass to children.
        let content_block_size = self.block_flow.fragment.style().content_block_size();

        let explicit_content_size =
            match (content_block_size, self.block_flow.base.block_container_explicit_block_size) {
            (LengthOrPercentageOrAuto::Percentage(percent), Some(container_size)) => {
                Some(container_size.scale_by(percent))
            }
            (LengthOrPercentageOrAuto::Percentage(_), None) |
            (LengthOrPercentageOrAuto::Auto, _) => None,
            (LengthOrPercentageOrAuto::Calc(_), _) => None,
            (LengthOrPercentageOrAuto::Length(length), _) => Some(length),
        };

        // FIXME (mbrubeck): Get correct mode for absolute containing block
        let containing_block_mode = self.block_flow.base.writing_mode;

        let mut iterator = self.block_flow.base.child_iter().enumerate().peekable();
        while let Some((_, kid)) = iterator.next() {
            {
                let kid_base = flow::mut_base(kid);
                kid_base.block_container_explicit_block_size = explicit_content_size;
            }

            // The inline-start margin edge of the child flow is at our inline-start content edge,
            // and its inline-size is our content inline-size.
            let kid_mode = flow::base(kid).writing_mode;
            {
                let kid_base = flow::mut_base(kid);
                if kid_base.flags.contains(INLINE_POSITION_IS_STATIC) {
                    kid_base.position.start.i =
                        if kid_mode.is_bidi_ltr() == containing_block_mode.is_bidi_ltr() {
                            inline_start_content_edge
                        } else {
                            // The kid's inline 'start' is at the parent's 'end'
                            inline_end_content_edge
                        };
                }
                kid_base.block_container_inline_size = content_inline_size;
                kid_base.block_container_writing_mode = containing_block_mode;
            }
        }
    }

    // TODO(zentner): This function should actually flex elements!
    // Currently, this is the core of InlineFlow::propagate_assigned_inline_size_to_children() with
    // fragment logic stripped out.
    fn inline_mode_assign_inline_sizes(&mut self,
                                       _layout_context: &LayoutContext,
                                       inline_start_content_edge: Au,
                                       _inline_end_content_edge: Au,
                                       content_inline_size: Au) {
        let _scope = layout_debug_scope!("flex::inline_mode_assign_inline_sizes");
        debug!("inline_mode_assign_inline_sizes");

        debug!("content_inline_size = {:?}", content_inline_size);

        let child_count = ImmutableFlowUtils::child_count(self as &Flow) as i32;
        debug!("child_count = {:?}", child_count);
        if child_count == 0 {
            return;
        }

        let even_content_inline_size = content_inline_size / child_count;

        let inline_size = self.block_flow.base.block_container_inline_size;
        let container_mode = self.block_flow.base.block_container_writing_mode;
        self.block_flow.base.position.size.inline = inline_size;

        let block_container_explicit_block_size = self.block_flow.base.block_container_explicit_block_size;
        let mut inline_child_start = inline_start_content_edge;
        for kid in self.block_flow.base.child_iter() {
            let kid_base = flow::mut_base(kid);

            kid_base.block_container_inline_size = even_content_inline_size;
            kid_base.block_container_writing_mode = container_mode;
            kid_base.block_container_explicit_block_size = block_container_explicit_block_size;
            kid_base.position.start.i = inline_child_start;
            inline_child_start = inline_child_start + even_content_inline_size;
        }
    }

    // TODO(zentner): This function should actually flex elements!
    fn block_mode_assign_block_size<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        self.block_flow.assign_block_size(layout_context)
    }

    // TODO(zentner): This function should actually flex elements!
    // Currently, this is the core of TableRowFlow::assign_block_size() with
    // float related logic stripped out.
    fn inline_mode_assign_block_size<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        let _scope = layout_debug_scope!("flex::inline_mode_assign_block_size");

        let mut max_block_size = Au(0);
        let thread_id = self.block_flow.base.thread_id;
        for kid in self.block_flow.base.child_iter() {
            kid.assign_block_size_for_inorder_child_if_necessary(layout_context, thread_id);

            {
                let child_fragment = &mut kid.as_mut_block().fragment;
                // TODO: Percentage block-size
                let child_specified_block_size =
                    MaybeAuto::from_style(child_fragment.style().content_block_size(),
                                          Au(0)).specified_or_zero();
                max_block_size =
                    max(max_block_size,
                        child_specified_block_size +
                        child_fragment.border_padding.block_start_end());
            }
            let child_node = flow::mut_base(kid);
            child_node.position.start.b = Au(0);
            max_block_size = max(max_block_size, child_node.position.size.block);
        }

        let mut block_size = max_block_size;
        // TODO: Percentage block-size

        block_size = match MaybeAuto::from_style(self.block_flow
                                                     .fragment
                                                     .style()
                                                     .content_block_size(),
                                                 Au(0)) {
            MaybeAuto::Auto => block_size,
            MaybeAuto::Specified(value) => max(value, block_size),
        };

        // Assign the block-size of own fragment
        let mut position = self.block_flow.fragment.border_box;
        position.size.block = block_size;
        self.block_flow.fragment.border_box = position;
        self.block_flow.base.position.size.block = block_size;

        // Assign the block-size of kid fragments, which is the same value as own block-size.
        for kid in self.block_flow.base.child_iter() {
            {
                let kid_fragment = &mut kid.as_mut_block().fragment;
                let mut position = kid_fragment.border_box;
                position.size.block = block_size;
                kid_fragment.border_box = position;
            }

            // Assign the child's block size.
            flow::mut_base(kid).position.size.block = block_size
        }
    }
}

impl Flow for FlexFlow {
    fn class(&self) -> FlowClass {
        FlowClass::Flex
    }

    fn as_mut_block(&mut self) -> &mut BlockFlow {
        &mut self.block_flow
    }

    fn mark_as_root(&mut self) {
        self.block_flow.mark_as_root();
    }

    fn bubble_inline_sizes(&mut self) {
        let _scope = layout_debug_scope!("flex::bubble_inline_sizes {:x}",
                                         self.block_flow.base.debug_id());

        // Flexbox Section 9.0: Generate anonymous flex items:
        // This part was handled in the flow constructor.

        // Flexbox Section 9.1: Re-order the flex items (and any absolutely positioned flex
        // container children) according to their order.
        // TODO(zentner): We need to re-order the items at some point. However, all the operations
        // here ignore order, so we can afford to do it later, if necessary.

        // `flex item`s (our children) cannot be floated. Furthermore, they all establish BFC's.
        // Therefore, we do not have to handle any floats here.

        let mut flags = self.block_flow.base.flags;
        flags.remove(HAS_LEFT_FLOATED_DESCENDANTS);
        flags.remove(HAS_RIGHT_FLOATED_DESCENDANTS);

        match self.main_mode {
            Mode::Inline => self.inline_mode_bubble_inline_sizes(),
            Mode::Block  => self.block_mode_bubble_inline_sizes()
        }

        // Although our children can't be floated, we can.
        match self.block_flow.fragment.style().get_box().float {
            float::T::none => {}
            float::T::left => flags.insert(HAS_LEFT_FLOATED_DESCENDANTS),
            float::T::right => flags.insert(HAS_RIGHT_FLOATED_DESCENDANTS),
        }
        self.block_flow.base.flags = flags
    }

    fn assign_inline_sizes(&mut self, layout_context: &LayoutContext) {
        let _scope = layout_debug_scope!("flex::assign_inline_sizes {:x}", self.block_flow.base.debug_id());
        debug!("assign_inline_sizes");

        if !self.block_flow.base.restyle_damage.intersects(REFLOW_OUT_OF_FLOW | REFLOW) {
            return
        }

        // Our inline-size was set to the inline-size of the containing block by the flow's parent.
        // Now compute the real value.
        let containing_block_inline_size = self.block_flow.base.block_container_inline_size;
        self.block_flow.compute_used_inline_size(layout_context, containing_block_inline_size);
        if self.block_flow.base.flags.is_float() {
            self.block_flow.float.as_mut().unwrap().containing_inline_size = containing_block_inline_size
        }

        // Move in from the inline-start border edge.
        let inline_start_content_edge = self.block_flow.fragment.border_box.start.i +
            self.block_flow.fragment.border_padding.inline_start;

        debug!("inline_start_content_edge = {:?}", inline_start_content_edge);

        let padding_and_borders = self.block_flow.fragment.border_padding.inline_start_end();

        // Distance from the inline-end margin edge to the inline-end content edge.
        let inline_end_content_edge =
            self.block_flow.fragment.margin.inline_end +
            self.block_flow.fragment.border_padding.inline_end;

        debug!("padding_and_borders = {:?}", padding_and_borders);
        debug!("self.block_flow.fragment.border_box.size.inline = {:?}",
               self.block_flow.fragment.border_box.size.inline);
        let content_inline_size = self.block_flow.fragment.border_box.size.inline - padding_and_borders;

        match self.main_mode {
            Mode::Inline =>
                self.inline_mode_assign_inline_sizes(layout_context,
                                                     inline_start_content_edge,
                                                     inline_end_content_edge,
                                                     content_inline_size),
            Mode::Block  =>
                self.block_mode_assign_inline_sizes(layout_context,
                                                    inline_start_content_edge,
                                                    inline_end_content_edge,
                                                    content_inline_size)
        }
    }

    fn assign_block_size<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        self.block_flow.assign_block_size(layout_context);
        match self.main_mode {
            Mode::Inline =>
                self.inline_mode_assign_block_size(layout_context),
            Mode::Block  =>
                self.block_mode_assign_block_size(layout_context)
        }
    }

    fn compute_absolute_position(&mut self, layout_context: &LayoutContext) {
        self.block_flow.compute_absolute_position(layout_context)
    }

    fn place_float_if_applicable<'a>(&mut self, layout_context: &'a LayoutContext<'a>) {
        self.block_flow.place_float_if_applicable(layout_context)
    }

    fn update_late_computed_inline_position_if_necessary(&mut self, inline_position: Au) {
        self.block_flow.update_late_computed_inline_position_if_necessary(inline_position)
    }

    fn update_late_computed_block_position_if_necessary(&mut self, block_position: Au) {
        self.block_flow.update_late_computed_block_position_if_necessary(block_position)
    }

    fn build_display_list(&mut self, layout_context: &LayoutContext) {
        self.build_display_list_for_flex(Box::new(DisplayList::new()), layout_context);

        if opts::get().validate_display_list_geometry {
            self.block_flow.base.validate_display_list_geometry();
        }
    }

    fn repair_style(&mut self, new_style: &Arc<ComputedValues>) {
        self.block_flow.repair_style(new_style)
    }

    fn compute_overflow(&self) -> Rect<Au> {
        self.block_flow.compute_overflow()
    }

    fn generated_containing_block_size(&self, flow: OpaqueFlow) -> LogicalSize<Au> {
        self.block_flow.generated_containing_block_size(flow)
    }

    fn iterate_through_fragment_border_boxes(&self,
                                             iterator: &mut FragmentBorderBoxIterator,
                                             level: i32,
                                             stacking_context_position: &Point2D<Au>) {
        self.block_flow.iterate_through_fragment_border_boxes(iterator, level, stacking_context_position);
    }

    fn mutate_fragments(&mut self, mutator: &mut FnMut(&mut Fragment)) {
        self.block_flow.mutate_fragments(mutator);
    }
}
