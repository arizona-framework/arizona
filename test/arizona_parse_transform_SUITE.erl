-module(arizona_parse_transform_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include("arizona.hrl").
-dialyzer({nowarn_function, to_bin_crash/1}).

-export([all/0, groups/0]).
-export([
    literal_list_attr_folds/1,
    literal_list_attr_matches_runtime/1,
    literal_list_attr_native_matches_runtime/1,
    literal_list_attr_empty/1,
    literal_list_content_folds/1,
    literal_list_content_native_matches_runtime/1,
    literal_list_in_conditional_and_each/1,
    literal_list_attr_diff_no_op/1,
    literal_list_content_local_slot/1,
    literal_list_mixed_not_folded/1,
    literal_list_raw_and_command_not_folded/1,
    literal_list_script_still_rejected/1,
    az_each_basic/1,
    az_each_inside_html/1,
    each_with_named_fun_ref/1,
    each_with_named_fun_ref_arity_2/1,
    each_named_fun_ref_non_element_rejected/1,
    each_named_fun_ref_arity_2_non_element_rejected/1,
    each_stream_control_flow_body_rejected_with_wrapper_advice/1,
    computed_element_tag_rejected_with_idiom/1,
    malformed_element_still_invalid_element/1,
    each_stream_control_flow_wrapper_fix_keys_per_item/1,
    each_with_remote_fun_ref/1,
    each_named_fun_ref_diffs/1,
    each_named_fun_ref_pattern_param/1,
    each_named_fun_ref_guard/1,
    each_named_fun_ref_prefix_body/1,
    each_named_fun_ref_shadows_caller_var/1,
    each_named_fun_ref_loc_names_callee/1,
    each_named_fun_ref_nested_each/1,
    each_named_fun_ref_nested_macros_render/1,
    each_named_fun_multi_clause_rejected/1,
    each_named_fun_wrong_arity_rejected/1,
    each_named_fun_undefined_rejected/1,
    each_named_fun_ref_private_local_strict/1,
    each_named_fun_same_module_ref/1,
    each_named_fun_imported_rejected/1,
    each_named_fun_variable_module_rejected/1,
    each_named_fun_ref_native/1,
    each_named_fun_ref_html_body_diffs/1,
    each_named_fun_ref_native_html/1,
    each_body_html_wrapped_diffs/1,
    each_body_two_arg_html_diffs/1,
    each_body_list_with_html_rejected/1,
    each_body_descriptor_rejected/1,
    each_body_case_rejected/1,
    each_body_if_rejected/1,
    each_body_runtime_binary_rejected/1,
    each_body_two_arg_value_rejected/1,
    each_body_list_with_descriptor_rejected/1,
    each_body_mixed_fragment_ok/1,
    each_body_conditional_child_diffs/1,
    each_single_root_flag/1,
    each_multi_root_no_single_root_flag/1,
    each_stream_no_single_root_flag/1,
    each_native_no_single_root_flag/1,
    stateless_with_atom_callback/1,
    stateless_with_fun_ref_callback/1,
    stateless_with_remote_fun_ref_callback/1,
    az_html_dynamic_attr/1,
    az_html_dynamic/1,
    az_html_nested/1,
    az_html_static/1,
    az_layout_mixed/1,
    az_layout/1,
    bare_binary_bool/1,
    bool_attr_false/1,
    bool_attr_true/1,
    bool_dynamic_false/1,
    bool_dynamic_true/1,
    bool_dynamic_value/1,
    boolean_attr/1,
    case_expression_dynamic/1,
    counter_pattern/1,
    cross_target_html_in_native/1,
    cross_target_native_in_html/1,
    cross_target_siblings_compile/1,
    cross_target_html_in_terminal/1,
    cross_target_terminal_in_html/1,
    terminal_target_marked/1,
    terminal_value_marked_for_escape/1,
    terminal_sanitizes_control_chars/1,
    terminal_renders_styled_ansi/1,
    terminal_custom_sgr_attr/1,
    terminal_fg_attr/1,
    terminal_unknown_style_rejected/1,
    terminal_unknown_false_style_rejected/1,
    terminal_unknown_attr_rejected/1,
    terminal_unknown_tag_rejected/1,
    terminal_known_tags_accepted/1,
    terminal_event_command_rejected/1,
    terminal_dynamic_attr_rejected/1,
    terminal_each_renders/1,
    deeply_nested/1,
    diff_integration/1,
    dynamic_only_child/1,
    empty_children/1,
    event_attrs_static/1,
    fingerprint_stability/1,
    fingerprint_avoids_phash2_collision/1,
    format_error/1,
    invalid_attribute_name_false/1,
    invalid_attribute_name/1,
    invalid_attribute_name_true/1,
    invalid_attribute_nested/1,
    invalid_attribute/1,
    invalid_child_empty_tuple/1,
    invalid_child_mixed/1,
    invalid_child_tuple_literal/1,
    invalid_each_fun_arity/1,
    invalid_each_multi_clause/1,
    invalid_each_not_fun/1,
    binary_element_tag_is_verbatim/1,
    binary_element_tag_classifies/1,
    non_name_literal_tag_still_invalid/1,
    invalid_element_four/1,
    invalid_element_single/1,
    iolist_dynamic_child/1,
    layout_bare_dynamic/1,
    layout_dynamic_attr/1,
    layout_mixed_dynamic_item/1,
    layout_mixed_list/1,
    layout_mixed_with_bare_dynamic/1,
    layout_multiple_dynamic_children/1,
    layout_nested_no_az/1,
    layout_render_integration/1,
    layout_single_element/1,
    layout_static_only/1,
    layout_void_dynamic_attr/1,
    auto_nodiff_inner_content_top_level/1,
    auto_nodiff_inner_content_nested/1,
    auto_nodiff_no_inner_content_no_diff_key/1,
    mixed_attr_forms/1,
    mixed_list_no_nodiff/1,
    multiple_stateful_calls/1,
    nested_elements/1,
    no_change_diff/1,
    nodiff_absent/1,
    nodiff_atom/1,
    nodiff_binary/1,
    nodiff_diff_integration/1,
    nodiff_each/1,
    nodiff_inherited_branch/1,
    nodiff_bare_branch_inherited/1,
    nodiff_slot_show_hide/1,
    nodiff_slot_switch/1,
    nodiff_slot_helper_switch/1,
    nodiff_slot_same_branch_kept/1,
    nodiff_slot_wrapper_switch/1,
    nodiff_slot_inner_change/1,
    nodiff_slot_az_key/1,
    nodiff_slot_az_key_spellings/1,
    nodiff_slot_child_views_not_updated/1,
    nodiff_slot_root_child_id/1,
    nodiff_slot_list_items/1,
    nodiff_slot_stream_items/1,
    nodiff_token_names_identity/1,
    nodiff_token_follows_same_nodiff/1,
    nodiff_token_item_key/1,
    nodiff_token_unkeyed_item/1,
    nodiff_token_only_on_nodiff_roots/1,
    nodiff_value_raises/1,
    nodiff_branch_swaps_away/1,
    nodiff_branch_same_template_skips_eval/1,
    nodiff_item_branch_swaps_away/1,
    nodiff_child_view_branch_swaps_away/1,
    nodiff_branch_local_across_swap/1,
    nodiff_same_statics_branches_swap/1,
    nodiff_root_child_kept_wherever_evaluated/1,
    nodiff_root_child_slot_leaves/1,
    nodiff_inherited_nested_html/1,
    nodiff_inherited_each/1,
    nodiff_inherited_each_in_each/1,
    nodiff_inherited_hoisted_each/1,
    nodiff_inherited_helper_each/1,
    nodiff_inherited_named_each/1,
    nodiff_each_item_inherited/1,
    nodiff_layout_inherited_each/1,
    nodiff_stateful_props_not_inherited/1,
    nodiff_inherited_local/1,
    nodiff_layout_branch_local/1,
    nodiff_attr_local_no_az/1,
    nodiff_local_slot_numbering/1,
    nodiff_fun_prop_not_inherited/1,
    nodiff_named_fun_prop_not_inherited/1,
    nodiff_inherited_named_each_helper/1,
    nodiff_inherited_hoisted_source/1,
    nodiff_each_stateful_item/1,
    nodiff_stream_az_key/1,
    nodiff_inherited_native/1,
    nodiff_inherited_raw_text/1,
    nodiff_live_root_hoisted_each/1,
    nodiff_call_not_compiled_error/1,
    nodiff_absent_keeps_nested_markers/1,
    nodiff_nested_element_rejected/1,
    nodiff_with_other_attrs/1,
    render_integration/1,
    shared_az/1,
    mixed_fragment_bare_value_gets_own_az/1,
    single_dynamic_attr/1,
    single_dynamic_text/1,
    single_expr_child/1,
    stateful_child_not_transformed/1,
    static_after_dynamic_attr/1,
    static_between_dynamics/1,
    static_only/1,
    template1_dynamic_expr_render/1,
    template1_dynamic_expr/1,
    template1_fragment_render/1,
    template1_fragment/1,
    template1_static_binary/1,
    template1_static_fragment/1,
    template2_basic/1,
    template2_compound_markers/1,
    template2_diff_integration/1,
    template2_dynamic_attr/1,
    template2_empty_list_render/1,
    template2_fragment_body/1,
    template2_fragment_multi_expr/1,
    template2_fragment_render/1,
    template2_guard/1,
    template2_inner_template1/1,
    template2_map_pattern_non_element/1,
    template2_map_pattern/1,
    template2_multi_expr_attr/1,
    template2_multi_expr_body/1,
    template2_multiple_children/1,
    template2_nested_in_template/1,
    template2_non_element_expr/1,
    template2_non_element_multi_expr/1,
    template2_non_element_render/1,
    template2_prefix_partial_use/1,
    template2_prefix_static_body/1,
    template2_render_integration/1,
    template2_static_and_dynamic/1,
    template2_static_binary_body/1,
    template2_static_item/1,
    template2_void_item/1,
    to_bin_crash/1,
    two_dynamic_attrs/1,
    two_dynamic_texts/1,
    utf8_cactus_emoji/1,
    utf8_each_static/1,
    utf8_mixed_ascii/1,
    utf8_render_integration/1,
    utf8_static_attr/1,
    utf8_static_child_emoji/1,
    variable_dynamic/1,
    inline_hoisted_text_read/1,
    inline_hoisted_attr_read/1,
    inline_maps_get_chain/1,
    inline_case_value/1,
    inline_case_of_case/1,
    inline_opaque_call/1,
    inline_side_effect_not_inlined/1,
    inline_diff_updates/1,
    inline_unrelated_key_skipped/1,
    inline_hoisted_root_id/1,
    inline_branch_bound_var/1,
    inline_underscore_collision/1,
    inline_lifted_pattern_var/1,
    inline_lifted_nested_match/1,
    inline_hoisted_attr_list_read/1,
    inline_each_hoisted_source/1,
    inline_distinct_slots_key_isolation/1,
    inline_multi_clause_render/1,
    inline_get_lazy_hoisted/1,
    inline_nested_html_shared_var/1,
    inline_same_var_two_slots/1,
    inline_non_bindings_var_name/1,
    inline_destructuring_not_inlined/1,
    inline_comprehension_source/1,
    inline_strict_generator_shadow/1,
    hoisted_var_captured_rejected/1,
    hoisted_var_captured_message/1,
    hoisted_var_pattern_reads_not_captured/1,
    hoisted_var_internal_binder_not_captured/1,
    hoisted_var_zip_pattern_binds/1,
    inline_hoisted_html_renders_and_tracks/1,
    inline_hoisted_html_nested_chain/1,
    inline_hoisted_each_renders_and_tracks/1,
    inline_hoisted_stateless_atom_renders_and_tracks/1,
    inline_hoisted_html_each_body_single_root/1,
    slot_tracks_only_reads_in_its_own_closure/1,
    each_param_rename_avoids_shadow_warning/1,
    inline_hoisted_html_unrelated_key_skipped/1,
    helper_zero_arity_inlines/1,
    helper_with_args_renders_and_tracks/1,
    helper_body_read_tracks/1,
    helper_body_var_shadows_caller_var/1,
    helper_whole_body_html_unwraps_and_tracks/1,
    helper_nested_calls_inline/1,
    helper_element_list_body_tracks/1,
    helper_in_each_item_body_inlines/1,
    helper_same_module_call_inlines/1,
    element_list_child_tracks/1,
    helper_multi_clause_error/1,
    helper_guarded_error/1,
    helper_params_not_vars_error/1,
    helper_body_not_single_expr_error/1,
    helper_recursive_error/1,
    helper_scalar_body_read_still_tracks/1,
    helper_multi_clause_scalar_untouched/1,
    helper_wrapped_multi_statement_untouched/1,
    tracked_get_submap_errors/1,
    tracked_get_submap_az_errors/1,
    tracked_get_lazy_submap_errors/1,
    tracked_get_maps_get_ok/1,
    tracked_get_each_item_ok/1,
    tracked_get_alias_ok/1,
    tracked_get_literal_map_ok/1,
    tracked_get_renamed_param_ok/1,
    tracked_get_with_projection_ok/1,
    tracked_get_with_submap_errors/1,
    tracked_get_case_aliased_bindings_flagged/1,
    with_handoff_freezes_without_tracking/1,
    with_handoff_tracks_outer_slot/1,
    with_handoff_hoisted_tracks_outer_slot/1,
    guard_case_tracked_read_tracks/1,
    guard_if_tracked_read_tracks/1,
    guard_buried_tracked_read_tracks/1,
    guard_hoisted_case_result_tracks/1,
    guard_tracked_read_also_in_body_tracks/1,
    guard_transitively_derived_read_tracks/1,
    guard_fun_clause_tracked_read_tracks/1,
    guard_attr_value_tracks/1,
    guard_nodiff_compiles/1,
    guard_pattern_var_ok/1,
    guard_untracked_local_ok/1,
    guard_tracked_read_in_scrutinee_ok/1,
    guard_pattern_bound_from_tracked_case_ok/1,
    void_element/1,
    void_in_each_with_children/1,
    void_nested_child_with_children/1,
    void_no_attrs/1,
    void_tag_case_insensitive/1,
    void_tag_case_insensitive_rejects_children/1,
    reserved_attr_case_insensitive/1,
    az_view_attr_case_insensitive/1,
    nodiff_directive_case_insensitive/1,
    void_two_element_dynamic_attr/1,
    void_two_element_tuple/1,
    void_with_dynamic_children/1,
    void_with_single_expr_child/1,
    void_with_static_children/1,
    az_view_auto_injected/1,
    az_view_explicit_on_root_ok/1,
    az_view_explicit_tuple_true/1,
    az_view_on_child_raises/1,
    az_view_in_stateless_raises/1,
    az_view_binary_form_raises/1,
    reserved_az_attr_raises/1,
    reserved_az_local_attr_raises/1,
    az_prefixed_user_attrs_allowed/1,
    az_view_case_in_render/1,
    az_view_if_in_render/1,
    az_view_try_in_render/1,
    az_view_receive_in_render/1,
    az_view_block_in_render/1,
    az_view_maybe_in_render/1,
    az_view_multi_clause_render/1,
    az_view_no_html_in_render/1,
    az_view_static_id_raises/1,
    az_view_composed_id_raises/1,
    local_content_render/1,
    local_attr_render/1,
    local_diff_skipped/1,
    local_key_not_literal_error/1,
    local_content_multi_slot/1,
    local_in_nodiff/1,
    local_descriptor_merge/1,
    local_attr_coexists_dynamic/1,
    local_key_reused_error/1,
    local_az_facade_render/1,
    local_attr_interp_render/1,
    local_attr_interp_affix/1,
    local_attr_multiple_error/1,
    local_attr_mixed_error/1,
    local_attr_interp_boolean/1,
    local_attr_interp_numeric/1,
    local_attr_interp_multi_static/1,
    local_in_native_error/1,
    local_content_in_raw_text_error/1,
    local_attr_in_raw_text_ok/1,
    local_in_each_renders/1,
    local_atom_key/1,
    local_atom_binary_reuse_error/1,
    local_orphaned_fragment_top_error/1,
    local_orphaned_conditional_branch_error/1,
    local_orphaned_whole_body_error/1,
    ssr_escapes_content_value/1,
    ssr_escapes_attr_value/1,
    ssr_escape_all_unsafe_chars/1,
    raw_opt_out_not_escaped/1,
    az_raw_opt_out_not_escaped/1,
    block_value_not_escaped/1,
    value_template_inner_escaped/1,
    diff_value_stays_raw/1,
    fingerprint_escapes_value/1,
    native_backend_not_escaped/1,
    static_binary_sized_integer_not_utf8/1,
    static_binary_utf8_and_ascii_still_fold/1
]).
-export([
    raw_text_script_bare_dynamic_rejected/1,
    raw_text_style_bare_dynamic_rejected/1,
    raw_text_conditional_dynamic_rejected/1,
    raw_text_helper_call_rejected/1,
    raw_text_static_only_still_compiles/1,
    raw_text_az_alias_raw_accepted/1,
    raw_text_reject_message_renders/1,
    raw_text_in_nodiff_bare_dynamic_rejected/1,
    raw_text_in_nodiff_raw_neutralized/1,
    raw_text_style_css_round_trip/1,
    raw_text_style_close_tag_still_neutralized/1,
    raw_text_hoisted_raw_boundary/1,
    raw_text_uppercase_tag_classified/1,
    raw_text_svg_title_diffable/1,
    raw_text_svg_title_factoring/1,
    raw_text_svg_title_local/1,
    raw_text_foreign_script_style_escaped/1,
    raw_text_script_markerless/1,
    raw_text_script_not_escaped/1,
    raw_text_style_markerless/1,
    raw_text_json_ld_raw_verbatim/1,
    raw_text_title_escaped_markerless/1,
    raw_text_textarea_markerless/1,
    raw_text_content_diffs_against_element_az/1,
    raw_text_nodiff_keeps_script_render_once/1,
    raw_text_dynamic_attr_still_diffable/1,
    normal_element_keeps_markers/1,
    raw_text_sibling_after_restores_markers/1,
    raw_text_sibling_before_keeps_markers/1,
    raw_text_between_normal_siblings_markers/1,
    raw_text_mixed_az_numbering_diffs/1,
    raw_text_two_content_slots/1,
    raw_text_static_text_around_slot/1,
    raw_text_conditional_value_markerless/1,
    raw_text_attr_diffs_content_render_once/1,
    raw_text_escapable_adjacent_to_raw/1,
    native_script_tag_unaffected/1
]).
-export([
    cond_case_bare_element_renders/1,
    cond_case_bare_matches_html/1,
    cond_if_bare_element/1,
    cond_nested_case_bare_element/1,
    cond_case_terminal_inherits_target/1,
    cond_case_diff_transition/1,
    cond_case_element_list_tail/1,
    cond_case_mixed_fragment_tail/1,
    list_child_with_each/1,
    list_child_with_stateless/1,
    list_child_with_nested_html/1,
    list_child_binary_key_map/1,
    list_child_data_map_not_template/1,
    list_child_guarded_helper_not_rejected/1,
    list_child_hoisted_html/1,
    list_child_helper_html/1,
    list_child_helper_each/1,
    helper_arg_each_native/1,
    helper_arg_each_terminal/1,
    helper_arg_each_svg/1,
    hoisted_each_native/1,
    hoisted_each_terminal/1,
    hoisted_each_svg/1,
    hoisted_each_helper_arg_svg/1,
    hoisted_cross_target_nesting/1,
    list_child_inner_content/1,
    list_child_raw_beside_value/1,
    list_child_local_orphaned/1,
    list_child_case_tail/1,
    list_child_stateful/1,
    list_child_terminal/1,
    list_child_nodiff/1,
    list_child_wrapped_helper_off_tail/1,
    wrapped_helper_try_tail_tracked/1,
    wrapped_helper_receive_tail_tracked/1,
    wrapped_helper_maybe_tail_tracked/1,
    list_child_wrapped_element_helper_off_tail/1,
    list_child_stream_insert/1,
    list_child_plain_each/1,
    list_child_case_item_markup/1,
    raw_text_rejects_each/1,
    raw_text_rejects_element_child/1,
    raw_text_rejects_inlined_element_helper/1,
    raw_text_keeps_text_template/1,
    raw_text_rejects_branch_markup/1,
    raw_text_keeps_values/1,
    raw_text_list_with_raw_is_text/1,
    markup_var_in_content_list_rejected/1,
    markup_var_in_content_list_allowed/1,
    cond_begin_block_tail/1,
    cond_try_bare_element/1,
    cond_maybe_bare_element/1,
    cond_receive_bare_element/1
]).
-export([scope_keeps_static_marker_text/1]).
-export([scope_keeps_static_marker_text_in_each_item/1]).
-export([scope_slot_keeps_static_marker_text/1]).
-export([scope_repeated_stateless_keeps_static_marker_text/1]).
-export([az_attrs_records_command_names/1]).
-export([az_attrs_skips_unproven_values/1]).
-export([az_attrs_keeps_prevent_default_with_a_value/1]).
-export([az_attrs_skips_injected_az_view/1]).
-export([az_attrs_records_component_modules/1]).
-export([improper_template_list_rejected/1]).
-export([improper_attr_value_is_text/1]).

all() ->
    [
        {group, elements},
        {group, local},
        {group, each},
        {group, integration},
        {group, escaping},
        {group, raw_text},
        {group, conditional_elements},
        {group, inline},
        {group, helper},
        {group, tracked_get},
        {group, tracked_guard},
        {group, layout},
        {group, utf8},
        {group, errors},
        {group, cross_target},
        {group, terminal},
        {group, az_macros},
        {group, az_view},
        {group, az_scoping},
        {group, az_attrs},
        {group, nodiff_slot},
        {group, literal_fold}
    ].

groups() ->
    [
        {literal_fold, [parallel], [
            literal_list_attr_folds,
            literal_list_attr_matches_runtime,
            literal_list_attr_native_matches_runtime,
            literal_list_attr_empty,
            literal_list_content_folds,
            literal_list_content_native_matches_runtime,
            literal_list_in_conditional_and_each,
            literal_list_attr_diff_no_op,
            literal_list_content_local_slot,
            literal_list_mixed_not_folded,
            literal_list_raw_and_command_not_folded,
            literal_list_script_still_rejected
        ]},
        {nodiff_slot, [parallel], [
            nodiff_slot_show_hide,
            nodiff_slot_switch,
            nodiff_slot_helper_switch,
            nodiff_slot_same_branch_kept,
            nodiff_slot_wrapper_switch,
            nodiff_slot_inner_change,
            nodiff_slot_az_key,
            nodiff_slot_az_key_spellings,
            nodiff_slot_child_views_not_updated,
            nodiff_slot_root_child_id,
            nodiff_slot_list_items,
            nodiff_slot_stream_items,
            nodiff_token_names_identity,
            nodiff_token_follows_same_nodiff,
            nodiff_token_item_key,
            nodiff_token_unkeyed_item,
            nodiff_token_only_on_nodiff_roots,
            nodiff_value_raises
        ]},
        %% Tests 1-25: basic element parsing
        {elements, [parallel], [
            static_only,
            single_dynamic_text,
            single_dynamic_attr,
            counter_pattern,
            nested_elements,
            void_element,
            boolean_attr,
            shared_az,
            mixed_fragment_bare_value_gets_own_az,
            event_attrs_static,
            empty_children,
            fingerprint_stability,
            fingerprint_avoids_phash2_collision,
            render_integration,
            diff_integration,
            two_dynamic_attrs,
            two_dynamic_texts,
            static_between_dynamics,
            case_expression_dynamic,
            variable_dynamic,
            stateful_child_not_transformed,
            multiple_stateful_calls,
            deeply_nested,
            dynamic_only_child,
            no_change_diff,
            void_no_attrs,
            void_tag_case_insensitive,
            void_tag_case_insensitive_rejects_children,
            reserved_attr_case_insensitive,
            az_view_attr_case_insensitive,
            nodiff_directive_case_insensitive,
            static_after_dynamic_attr
        ]},
        %% Client-owned slots (?local)
        {local, [parallel], [
            local_content_render,
            local_attr_render,
            local_diff_skipped,
            local_key_not_literal_error,
            local_content_multi_slot,
            local_in_nodiff,
            local_descriptor_merge,
            local_attr_coexists_dynamic,
            local_key_reused_error,
            local_az_facade_render,
            local_attr_interp_render,
            local_attr_interp_affix,
            local_attr_multiple_error,
            local_attr_mixed_error,
            local_attr_interp_boolean,
            local_attr_interp_numeric,
            local_attr_interp_multi_static,
            local_in_native_error,
            local_content_in_raw_text_error,
            local_attr_in_raw_text_ok,
            local_in_each_renders,
            local_atom_key,
            local_atom_binary_reuse_error,
            local_orphaned_fragment_top_error,
            local_orphaned_conditional_branch_error,
            local_orphaned_whole_body_error
        ]},
        %% Tests 26-39: template/2 (each) tests
        {each, [parallel], [
            template2_basic,
            template2_dynamic_attr,
            template2_multiple_children,
            template2_nested_in_template,
            template2_static_and_dynamic,
            template2_render_integration,
            template2_compound_markers,
            template2_multi_expr_body,
            template2_multi_expr_attr,
            template2_guard,
            template2_static_item,
            template2_void_item,
            template2_inner_template1,
            template2_empty_list_render,
            each_with_named_fun_ref,
            each_with_named_fun_ref_arity_2,
            each_named_fun_ref_non_element_rejected,
            each_named_fun_ref_arity_2_non_element_rejected,
            each_stream_control_flow_body_rejected_with_wrapper_advice,
            computed_element_tag_rejected_with_idiom,
            malformed_element_still_invalid_element,
            each_stream_control_flow_wrapper_fix_keys_per_item,
            each_with_remote_fun_ref,
            each_named_fun_ref_diffs,
            each_named_fun_ref_pattern_param,
            each_named_fun_ref_guard,
            each_named_fun_ref_prefix_body,
            each_named_fun_ref_shadows_caller_var,
            each_named_fun_ref_loc_names_callee,
            each_named_fun_ref_nested_each,
            each_named_fun_ref_nested_macros_render,
            each_named_fun_multi_clause_rejected,
            each_named_fun_wrong_arity_rejected,
            each_named_fun_undefined_rejected,
            each_named_fun_ref_private_local_strict,
            each_named_fun_same_module_ref,
            each_named_fun_imported_rejected,
            each_named_fun_variable_module_rejected,
            each_named_fun_ref_native,
            each_named_fun_ref_html_body_diffs,
            each_named_fun_ref_native_html,
            each_body_html_wrapped_diffs,
            each_body_two_arg_html_diffs,
            each_body_list_with_html_rejected,
            each_body_descriptor_rejected,
            each_body_case_rejected,
            each_body_if_rejected,
            each_body_runtime_binary_rejected,
            each_body_two_arg_value_rejected,
            each_body_list_with_descriptor_rejected,
            each_body_mixed_fragment_ok,
            each_body_conditional_child_diffs,
            each_single_root_flag,
            each_multi_root_no_single_root_flag,
            each_stream_no_single_root_flag,
            each_native_no_single_root_flag,
            stateless_with_atom_callback,
            stateless_with_fun_ref_callback,
            stateless_with_remote_fun_ref_callback
        ]},
        %% Tests 40-67: integration, fragments, patterns, nodiff, bool attrs
        {integration, [parallel], [
            template2_diff_integration,
            template1_fragment,
            template1_fragment_render,
            template2_non_element_expr,
            template2_non_element_render,
            template2_static_binary_body,
            template2_non_element_multi_expr,
            template1_static_binary,
            template1_dynamic_expr,
            template1_dynamic_expr_render,
            template1_static_fragment,
            template2_fragment_body,
            template2_fragment_render,
            template2_fragment_multi_expr,
            template2_map_pattern,
            template2_map_pattern_non_element,
            template2_prefix_static_body,
            template2_prefix_partial_use,
            nodiff_atom,
            nodiff_binary,
            nodiff_absent,
            nodiff_diff_integration,
            nodiff_nested_element_rejected,
            bool_attr_true,
            bool_attr_false,
            bare_binary_bool,
            bool_dynamic_true,
            bool_dynamic_false,
            bool_dynamic_value,
            nodiff_with_other_attrs,
            nodiff_each,
            nodiff_inherited_branch,
            nodiff_bare_branch_inherited,
            nodiff_branch_swaps_away,
            nodiff_branch_same_template_skips_eval,
            nodiff_item_branch_swaps_away,
            nodiff_child_view_branch_swaps_away,
            nodiff_branch_local_across_swap,
            nodiff_same_statics_branches_swap,
            nodiff_root_child_kept_wherever_evaluated,
            nodiff_root_child_slot_leaves,
            nodiff_inherited_nested_html,
            nodiff_inherited_each,
            nodiff_inherited_each_in_each,
            nodiff_inherited_hoisted_each,
            nodiff_inherited_helper_each,
            nodiff_inherited_named_each,
            nodiff_each_item_inherited,
            nodiff_layout_inherited_each,
            nodiff_stateful_props_not_inherited,
            nodiff_inherited_local,
            nodiff_layout_branch_local,
            nodiff_attr_local_no_az,
            nodiff_local_slot_numbering,
            nodiff_fun_prop_not_inherited,
            nodiff_named_fun_prop_not_inherited,
            nodiff_inherited_named_each_helper,
            nodiff_inherited_hoisted_source,
            nodiff_each_stateful_item,
            nodiff_stream_az_key,
            nodiff_inherited_native,
            nodiff_inherited_raw_text,
            nodiff_live_root_hoisted_each,
            nodiff_call_not_compiled_error,
            nodiff_absent_keeps_nested_markers,
            mixed_attr_forms
        ]},
        %% HTML auto-escaping: value interpolations are escaped at the HTML
        %% boundary; blocks/raw/native are not; the live-diff path stays raw.
        {escaping, [parallel], [
            ssr_escapes_content_value,
            ssr_escapes_attr_value,
            ssr_escape_all_unsafe_chars,
            raw_opt_out_not_escaped,
            az_raw_opt_out_not_escaped,
            block_value_not_escaped,
            value_template_inner_escaped,
            diff_value_stays_raw,
            fingerprint_escapes_value,
            native_backend_not_escaped,
            static_binary_sized_integer_not_utf8,
            static_binary_utf8_and_ascii_still_fold
        ]},
        %% Raw-text elements (script/style/textarea/title): a dynamic content
        %% slot is markerless/render-once -- HTML comment diff markers would
        %% become literal content and corrupt the script/CSS/JSON-LD/title.
        {raw_text, [parallel], [
            raw_text_script_bare_dynamic_rejected,
            raw_text_style_bare_dynamic_rejected,
            raw_text_conditional_dynamic_rejected,
            raw_text_helper_call_rejected,
            raw_text_static_only_still_compiles,
            raw_text_az_alias_raw_accepted,
            raw_text_reject_message_renders,
            raw_text_in_nodiff_bare_dynamic_rejected,
            raw_text_in_nodiff_raw_neutralized,
            raw_text_style_css_round_trip,
            raw_text_style_close_tag_still_neutralized,
            raw_text_hoisted_raw_boundary,
            raw_text_uppercase_tag_classified,
            raw_text_svg_title_diffable,
            raw_text_svg_title_factoring,
            raw_text_svg_title_local,
            raw_text_foreign_script_style_escaped,
            raw_text_script_markerless,
            raw_text_script_not_escaped,
            raw_text_style_markerless,
            raw_text_json_ld_raw_verbatim,
            raw_text_title_escaped_markerless,
            raw_text_textarea_markerless,
            raw_text_content_diffs_against_element_az,
            raw_text_nodiff_keeps_script_render_once,
            raw_text_dynamic_attr_still_diffable,
            normal_element_keeps_markers,
            raw_text_sibling_after_restores_markers,
            raw_text_sibling_before_keeps_markers,
            raw_text_between_normal_siblings_markers,
            raw_text_mixed_az_numbering_diffs,
            raw_text_two_content_slots,
            raw_text_static_text_around_slot,
            raw_text_conditional_value_markerless,
            raw_text_attr_diffs_content_render_once,
            raw_text_escapable_adjacent_to_raw,
            native_script_tag_unaffected
        ]},
        %% Bare element tuples as case/if/begin branch results -- compiled into
        %% nested templates (no ?html wrap), inheriting the enclosing target.
        {conditional_elements, [parallel], [
            cond_case_bare_element_renders,
            cond_case_bare_matches_html,
            cond_if_bare_element,
            cond_nested_case_bare_element,
            cond_case_terminal_inherits_target,
            cond_case_diff_transition,
            cond_case_element_list_tail,
            cond_case_mixed_fragment_tail,
            list_child_with_each,
            list_child_with_stateless,
            list_child_with_nested_html,
            list_child_binary_key_map,
            list_child_data_map_not_template,
            list_child_guarded_helper_not_rejected,
            list_child_hoisted_html,
            list_child_helper_html,
            list_child_helper_each,
            helper_arg_each_native,
            helper_arg_each_terminal,
            helper_arg_each_svg,
            hoisted_each_native,
            hoisted_each_terminal,
            hoisted_each_svg,
            hoisted_each_helper_arg_svg,
            hoisted_cross_target_nesting,
            list_child_inner_content,
            list_child_raw_beside_value,
            list_child_local_orphaned,
            list_child_case_tail,
            list_child_stateful,
            list_child_terminal,
            list_child_nodiff,
            list_child_wrapped_helper_off_tail,
            wrapped_helper_try_tail_tracked,
            wrapped_helper_receive_tail_tracked,
            wrapped_helper_maybe_tail_tracked,
            list_child_wrapped_element_helper_off_tail,
            list_child_stream_insert,
            list_child_plain_each,
            list_child_case_item_markup,
            raw_text_rejects_each,
            raw_text_rejects_element_child,
            raw_text_rejects_inlined_element_helper,
            raw_text_keeps_text_template,
            raw_text_rejects_branch_markup,
            raw_text_keeps_values,
            raw_text_list_with_raw_is_text,
            markup_var_in_content_list_rejected,
            markup_var_in_content_list_allowed,
            cond_begin_block_tail,
            cond_try_bare_element,
            cond_maybe_bare_element,
            cond_receive_bare_element
        ]},
        %% Binding-read inlining: reads hoisted out of ?html still track per-slot
        {inline, [parallel], [
            inline_hoisted_text_read,
            inline_hoisted_attr_read,
            inline_maps_get_chain,
            inline_case_value,
            inline_case_of_case,
            inline_opaque_call,
            inline_side_effect_not_inlined,
            inline_diff_updates,
            inline_unrelated_key_skipped,
            inline_hoisted_root_id,
            inline_branch_bound_var,
            inline_underscore_collision,
            inline_lifted_pattern_var,
            inline_lifted_nested_match,
            inline_hoisted_attr_list_read,
            inline_each_hoisted_source,
            inline_distinct_slots_key_isolation,
            inline_multi_clause_render,
            inline_get_lazy_hoisted,
            inline_nested_html_shared_var,
            inline_same_var_two_slots,
            inline_non_bindings_var_name,
            inline_destructuring_not_inlined,
            inline_comprehension_source,
            inline_strict_generator_shadow,
            hoisted_var_captured_rejected,
            hoisted_var_captured_message,
            hoisted_var_pattern_reads_not_captured,
            hoisted_var_internal_binder_not_captured,
            hoisted_var_zip_pattern_binds,
            inline_hoisted_html_renders_and_tracks,
            inline_hoisted_html_nested_chain,
            inline_hoisted_each_renders_and_tracks,
            inline_hoisted_stateless_atom_renders_and_tracks,
            inline_hoisted_html_each_body_single_root,
            slot_tracks_only_reads_in_its_own_closure,
            each_param_rename_avoids_shadow_warning,
            inline_hoisted_html_unrelated_key_skipped
        ]},
        %% Local element-returning helper calls inlined at the template call site
        {helper, [parallel], [
            helper_zero_arity_inlines,
            helper_with_args_renders_and_tracks,
            helper_body_read_tracks,
            helper_body_var_shadows_caller_var,
            helper_whole_body_html_unwraps_and_tracks,
            helper_nested_calls_inline,
            helper_element_list_body_tracks,
            helper_in_each_item_body_inlines,
            helper_same_module_call_inlines,
            element_list_child_tracks,
            helper_multi_clause_error,
            helper_guarded_error,
            helper_params_not_vars_error,
            helper_body_not_single_expr_error,
            helper_recursive_error,
            helper_scalar_body_read_still_tracks,
            helper_multi_clause_scalar_untouched,
            helper_wrapped_multi_statement_untouched
        ]},
        %% Reject the tracked accessor reading a non-bindings (sub-)map
        {tracked_get, [parallel], [
            tracked_get_submap_errors,
            tracked_get_submap_az_errors,
            tracked_get_lazy_submap_errors,
            tracked_get_maps_get_ok,
            tracked_get_each_item_ok,
            tracked_get_alias_ok,
            tracked_get_literal_map_ok,
            tracked_get_renamed_param_ok,
            tracked_get_with_projection_ok,
            tracked_get_with_submap_errors,
            tracked_get_case_aliased_bindings_flagged,
            with_handoff_freezes_without_tracking,
            with_handoff_tracks_outer_slot,
            with_handoff_hoisted_tracks_outer_slot
        ]},
        %% Auto-track a binding read used in a template guard (the read can't live in the
        %% guard, so the transform injects a tracking touch so the slot stays reactive)
        {tracked_guard, [parallel], [
            guard_case_tracked_read_tracks,
            guard_if_tracked_read_tracks,
            guard_buried_tracked_read_tracks,
            guard_hoisted_case_result_tracks,
            guard_tracked_read_also_in_body_tracks,
            guard_transitively_derived_read_tracks,
            guard_fun_clause_tracked_read_tracks,
            guard_attr_value_tracks,
            guard_nodiff_compiles,
            guard_pattern_var_ok,
            guard_untracked_local_ok,
            guard_tracked_read_in_scrutinee_ok,
            guard_pattern_bound_from_tracked_case_ok
        ]},
        %% Tests 68-77c: layout tests
        {layout, [parallel], [
            layout_single_element,
            layout_mixed_list,
            layout_dynamic_attr,
            layout_render_integration,
            layout_static_only,
            layout_multiple_dynamic_children,
            layout_void_dynamic_attr,
            layout_bare_dynamic,
            layout_mixed_dynamic_item,
            layout_nested_no_az,
            layout_mixed_with_bare_dynamic,
            auto_nodiff_inner_content_top_level,
            auto_nodiff_inner_content_nested,
            auto_nodiff_no_inner_content_no_diff_key,
            mixed_list_no_nodiff
        ]},
        %% Tests 78-82: UTF-8 tests
        {utf8, [parallel], [
            utf8_static_child_emoji,
            utf8_cactus_emoji,
            utf8_static_attr,
            utf8_mixed_ascii,
            utf8_each_static,
            utf8_render_integration
        ]},
        %% Tests 83-107: error and validation tests
        {errors, [parallel], [
            iolist_dynamic_child,
            void_two_element_tuple,
            void_two_element_dynamic_attr,
            single_expr_child,
            to_bin_crash,
            void_with_static_children,
            void_with_dynamic_children,
            invalid_attribute,
            format_error,
            void_with_single_expr_child,
            void_nested_child_with_children,
            void_in_each_with_children,
            invalid_attribute_nested,
            binary_element_tag_is_verbatim,
            binary_element_tag_classifies,
            non_name_literal_tag_still_invalid,
            invalid_each_fun_arity,
            invalid_each_not_fun,
            invalid_attribute_name,
            invalid_each_multi_clause,
            invalid_attribute_name_true,
            invalid_attribute_name_false,
            invalid_element_single,
            invalid_element_four,
            invalid_child_empty_tuple,
            invalid_child_tuple_literal,
            invalid_child_mixed,
            improper_template_list_rejected,
            improper_attr_value_is_text
        ]},
        %% Inline cross-target nesting guard (?html in ?native and vice-versa)
        {cross_target, [parallel], [
            cross_target_html_in_native,
            cross_target_native_in_html,
            cross_target_siblings_compile,
            cross_target_html_in_terminal,
            cross_target_terminal_in_html
        ]},
        %% Terminal (ANSI) render target
        {terminal, [parallel], [
            terminal_target_marked,
            terminal_value_marked_for_escape,
            terminal_sanitizes_control_chars,
            terminal_renders_styled_ansi,
            terminal_custom_sgr_attr,
            terminal_fg_attr,
            terminal_unknown_style_rejected,
            terminal_unknown_false_style_rejected,
            terminal_unknown_attr_rejected,
            terminal_unknown_tag_rejected,
            terminal_known_tags_accepted,
            terminal_event_command_rejected,
            terminal_dynamic_attr_rejected,
            terminal_each_renders
        ]},
        %% Tests 108-115: az:html and az:each equivalence tests
        {az_macros, [parallel], [
            az_html_static,
            az_html_dynamic,
            az_html_dynamic_attr,
            az_each_basic,
            az_each_inside_html,
            az_layout,
            az_layout_mixed,
            az_html_nested
        ]},
        %% Tests 116-128: az-view auto-injection and validation in arizona_stateful render/1
        {az_view, [parallel], [
            az_view_auto_injected,
            az_view_explicit_on_root_ok,
            az_view_explicit_tuple_true,
            az_view_on_child_raises,
            az_view_in_stateless_raises,
            az_view_binary_form_raises,
            reserved_az_attr_raises,
            reserved_az_local_attr_raises,
            az_prefixed_user_attrs_allowed,
            az_view_case_in_render,
            az_view_if_in_render,
            az_view_try_in_render,
            az_view_receive_in_render,
            az_view_block_in_render,
            az_view_maybe_in_render,
            az_view_multi_clause_render,
            az_view_no_html_in_render,
            az_view_static_id_raises,
            az_view_composed_id_raises
        ]},
        %% Fingerprint/slot `az` scoping vs. user-authored marker lookalikes
        {az_scoping, [parallel], [
            scope_keeps_static_marker_text,
            scope_keeps_static_marker_text_in_each_item,
            scope_slot_keeps_static_marker_text,
            scope_repeated_stateless_keeps_static_marker_text
        ]},
        {az_attrs, [parallel], [
            az_attrs_records_command_names,
            az_attrs_skips_unproven_values,
            az_attrs_keeps_prevent_default_with_a_value,
            az_attrs_skips_injected_az_view,
            az_attrs_records_component_modules
        ]}
    ].

%% ============================================================================
%% Test Helper
%% ============================================================================

compile_module(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = split_forms(Tokens),
    ParsedForms = [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- Forms
    ],
    TransformedForms = arizona_parse_transform:parse_transform(ParsedForms, []),
    {ok, Mod, Bin} = compile:forms(TransformedForms, [return_errors]),
    {module, Mod} = code:load_binary(Mod, "", Bin),
    Mod.

%% Render a template and assert it carries none of the framework's markers.
%% The `az-nodiff` attribute a nodiff root keeps (its az-nodiff element token, or bare) is not
%% a diff anchor; it is stripped here so these cases pin the markerless markup, and
%% the `nodiff_token_attr`
%% group pins the attribute itself.
render_markerless(Tmpl) ->
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Tmpl)),
    ?assertEqual(nomatch, binary:match(HTML, [<<" az=\"">>, <<"<!--az:">>, <<"<!--/az-->">>])),
    strip_nodiff_attr(HTML).

strip_nodiff_attr(HTML) ->
    re:replace(HTML, ~" az-nodiff(=\"[^\"]*\")?", ~"", [global, {return, binary}]).

split_forms(Tokens) ->
    split_forms(Tokens, [], []).

split_forms([], [], Acc) ->
    lists:reverse(Acc);
split_forms([], Current, Acc) ->
    lists:reverse([lists:reverse(Current) | Acc]);
split_forms([{dot, _} = Dot | Rest], Current, Acc) ->
    split_forms(Rest, [], [lists:reverse([Dot | Current]) | Acc]);
split_forms([Token | Rest], Current, Acc) ->
    split_forms(Rest, [Token | Current], Acc).

assert_parse_error(Source, MatchFun) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = split_forms(Tokens),
    ParsedForms = [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- Forms
    ],
    case arizona_parse_transform:parse_transform(ParsedForms, []) of
        {error, [{_File, [{_Line, arizona_parse_transform, Reason}]}], []} ->
            ?assert(MatchFun(Reason));
        _ ->
            error(expected_parse_error)
    end.

%% Extract and JSON-decode the (HTML-attribute-escaped) `az-local` descriptor
%% baked onto the element in rendered HTML.
decode_local_descriptor(HTML) ->
    [_, Rest] = binary:split(HTML, <<"az-local=\"">>),
    [DescEsc | _] = binary:split(Rest, <<"\"">>),
    Desc = binary:replace(DescEsc, <<"&quot;">>, <<"\"">>, [global]),
    json:decode(Desc).

%% Scope helpers -- account for fingerprint-prefixed az values
scope_s(Fp, Statics) ->
    [scope_static(Fp, S) || S <- Statics].

scope_static(Fp, S) ->
    S1 = binary:replace(S, <<" az=\"">>, <<" az=\"", Fp/binary, "-">>, [global]),
    binary:replace(S1, <<"<!--az:">>, <<"<!--az:", Fp/binary, "-">>, [global]).

%% ============================================================================
%% Tests
%% ============================================================================

%% ?local: content slot renders the initial value, keeps az, carries the
%% az-local descriptor, and the dynamic evaluates to the bind-map.
local_content_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_content). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], [arizona_template:local(<<\"title\">>, <<\"Hello\">>)]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    Fp = maps:get(f, Tmpl),
    [{Az0, Fun, _Loc}] = maps:get(d, Tmpl),
    ?assertEqual(<<Fp/binary, "-0">>, Az0),
    ?assertEqual(#{diff => false, az_local => ~"title", v => ~"Hello"}, Fun()),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<Fp/binary, "-0">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"az-local=")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"Hello")).

%% ?local: attribute slot renders the initial value and the dynamic evaluates to
%% a bind-map carrying target = {attr, Name}.
local_attr_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{'data-active', arizona_template:local(<<\"tab\">>, <<\"home\">>)}], "
        "            [<<\"x\">>]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    [{_Az, Fun, _Loc}] = maps:get(d, Tmpl),
    ?assertEqual(
        #{diff => false, az_local => ~"tab", target => {attr, ~"data-active"}, v => ~"home"},
        Fun()
    ),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"data-active=")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"home")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"az-local=")).

%% An explicit-size element carries its own byte layout: `<<1024:16>>` is <<4,0>>,
%% not U+0400's UTF-8 (`<<208,128>>`). It must not fold; it renders through the
%% dynamic path, which preserves the bytes. Discriminates: the pre-fix static path
%% baked <<208,128>> and no <<4,0>>.
static_binary_sized_integer_not_utf8(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_static_sized). "
        "-export([render/1]). "
        "render(Bindings) -> arizona_template:html({'p', [], [<<1024:16>>]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<4, 0>>)),
    ?assertEqual(nomatch, binary:match(HTML, <<208, 128>>)).

%% Guard against over-rejection: a `[utf8]` element (`~"..."`, `X/utf8`) and an
%% all-ASCII `default` element still fold to a static (no dynamic-slot markers),
%% and a UTF-8 literal keeps its two bytes -- that was already correct and stays.
static_binary_utf8_and_ascii_still_fold(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_static_ok). "
        "-export([render/1]). "
        "render(Bindings) -> arizona_template:html({'p', [], [<<\"caf\", 233/utf8>>]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(<<"<p>caf", 195, 169, "</p>">>, HTML).

%% ?local: the slot is never diffed -- even when the init's binding source
%% changes between renders, no op is emitted (the bind-map is #{diff := false}).
local_diff_skipped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_skip). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], [arizona_template:local(<<\"n\">>, "
        "            arizona_template:get(count, Bindings, 0))]}"
        "    ). "
    ),
    T1 = Mod:render(#{count => 1}),
    {_HTML, Snap} = arizona_render:render(T1),
    T2 = Mod:render(#{count => 2}),
    {Ops, _Snap2} = arizona_diff:diff(T2, Snap),
    ?assertEqual([], Ops).

%% ?local: a non-literal key is a compile-time error.
local_key_not_literal_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_badkey). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], [arizona_template:local(Bindings, <<\"x\">>)]}"
        "    ). ",
        fun(R) -> R =:= local_key_not_literal end
    ).

%% ?local: multiple content slots in one element, mixed with static text and a
%% normal dynamic child. Each content local is keyed by its dynamic-slot index;
%% the ?get consumes slot 1, so the second local lands at slot 2. Each renders
%% its own comment-marked slot.
local_content_multi_slot(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [arizona_template:local(<<\"a\">>, <<\"A\">>), "
        "                   <<\"foo\">>, "
        "                   arizona_template:get(x, Bindings, <<\"X\">>), "
        "                   arizona_template:local(<<\"b\">>, <<\"B\">>)]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(#{~"c" => #{~"0" => ~"a", ~"2" => ~"b"}}, decode_local_descriptor(HTML)),
    %% Three comment-marked dynamic slots: local a, the ?get, local b.
    ?assertEqual(3, length(binary:matches(HTML, ~"<!--az:"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"A")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"B")).

%% ?local in an az-nodiff template: only the element holding a content local gets an
%% `az` and that slot's marker -- what the client resolves it by. Everything else
%% stays markerless.
local_in_nodiff(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            {'span', [], [arizona_template:local(<<\"k\">>, <<\"x\">>)]},"
        "            {'p', [], [arizona_template:get(a, Bindings)]}"
        "        ]}"
        "    ). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{a => ~"A"}))),
    ?assertEqual(#{~"c" => #{~"0" => ~"k"}}, decode_local_descriptor(HTML)),
    ?assertEqual(1, length(binary:matches(HTML, <<" az=\"">>))),
    ?assertEqual(1, length(binary:matches(HTML, <<"<!--az:">>))),
    ?assertMatch({_, _}, binary:match(HTML, <<"-->x<!--/az--></span><p>A</p></div>">>)).

%% ?local: multiple attribute binds AND a content bind on one element merge into
%% a single az-local descriptor.
local_descriptor_merge(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_merge). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', "
        "            [{title, arizona_template:local(<<\"t\">>, <<\"\">>)}, "
        "             {lang, arizona_template:local(<<\"l\">>, <<\"en\">>)}], "
        "            [arizona_template:local(<<\"c\">>, <<\"hi\">>)]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(
        #{~"a" => #{~"title" => ~"t", ~"lang" => ~"l"}, ~"c" => #{~"0" => ~"c"}},
        decode_local_descriptor(HTML)
    ).

%% ?local: a bound attribute and a normal dynamic attribute coexist on one
%% element -- the normal attr still diffs, the bound attr is skipped.
local_attr_coexists_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_coexist). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', "
        "            [{open, arizona_template:local(<<\"o\">>, false)}, "
        "             {class, arizona_template:get(cls, Bindings, <<\"a\">>)}], "
        "            [<<\"x\">>]}"
        "    ). "
    ),
    T1 = Mod:render(#{cls => <<"a">>}),
    {_HTML, Snap} = arizona_render:render(T1),
    T2 = Mod:render(#{cls => <<"b">>}),
    {Ops, _Snap2} = arizona_diff:diff(T2, Snap),
    %% Only the dynamic class produces an op (OP_SET_ATTR = 1); open is skipped.
    ?assertMatch([[1, _, <<"class">>, <<"b">>]], Ops).

%% ?local: a key cannot bind both content and an attribute on the same element.
local_key_reused_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_reuse). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [{title, arizona_template:local(<<\"x\">>, <<\"\">>)}], "
        "            [arizona_template:local(<<\"x\">>, <<\"y\">>)]}"
        "    ). ",
        fun(R) -> R =:= local_key_reused end
    ).

%% ?local: the az:local/2 facade is recognized by the parse transform exactly
%% like arizona_template:local/2 -- in both attribute and content positions.
local_az_facade_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_az_facade). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [{title, az:local(<<\"t\">>, <<\"hi\">>)}], "
        "            [az:local(<<\"c\">>, <<\"body\">>)]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    Results = [F() || {_Az, F, _Loc} <:- maps:get(d, Tmpl)],
    ?assert(
        lists:member(
            #{diff => false, az_local => ~"t", target => {attr, ~"title"}, v => ~"hi"}, Results
        )
    ),
    ?assert(lists:member(#{diff => false, az_local => ~"c", v => ~"body"}, Results)),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"az-local=")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"body")).

%% ?local interpolated into an attribute value (static prefix + one local): SSR
%% renders the composed value, and the descriptor carries the key plus the
%% prefix/suffix (`ap`) the client recomposes from.
local_attr_interp_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_attr_interp). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [{class, [<<\"foo \">>, arizona_template:local(<<\"x\">>, <<\"bar\">>)]}], "
        "            [<<\"hi\">>]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"class=\"foo bar\"")),
    ?assertEqual(
        #{~"a" => #{~"class" => ~"x"}, ~"ap" => #{~"class" => [~"foo ", ~""]}},
        decode_local_descriptor(HTML)
    ).

%% Interpolation with both a prefix and a suffix around the local.
local_attr_interp_affix(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_attr_affix). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'a', [{href, [<<\"/u/\">>, arizona_template:local(<<\"id\">>, <<\"1\">>), "
        "            <<\"/edit\">>]}], [<<\"e\">>]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"href=\"/u/1/edit\"")),
    ?assertEqual(
        #{~"a" => #{~"href" => ~"id"}, ~"ap" => #{~"href" => [~"/u/", ~"/edit"]}},
        decode_local_descriptor(HTML)
    ).

%% More than one ?local in a single attribute value can't be recomposed -- error.
local_attr_multiple_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_attr_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [{class, [arizona_template:local(<<\"a\">>, <<\"\">>), <<\"-\">>, "
        "            arizona_template:local(<<\"b\">>, <<\"\">>)]}], [<<\"x\">>]}"
        "    ). ",
        fun(R) -> R =:= local_attr_multiple end
    ).

%% A ?local mixed with a server-owned dynamic in one attribute value -- error.
local_attr_mixed_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_attr_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [{class, [<<\"x\">>, arizona_template:local(<<\"a\">>, <<\"\">>), "
        "            arizona_template:get(y, Bindings)]}], [<<\"z\">>]}"
        "    ). ",
        fun(R) -> R =:= local_attr_mixed end
    ).

%% Interpolating a boolean attribute is a documented footgun: an interpolated
%% value always renders name="value", so a false init becomes disabled="false"
%% (still present in HTML), not absent as a whole-value ?local would render it.
local_attr_interp_boolean(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_attr_bool). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'button', [{disabled, [<<\"\">>, arizona_template:local(<<\"d\">>, false)]}], "
        "            [<<\"x\">>]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"disabled=\"false\"")).

%% A non-binary init (here an integer) composes as its text, not a raw iolist
%% byte -- exercises the to_bin/1 wrap in the interpolated bind-map's v.
local_attr_interp_numeric(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_attr_num). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'li', [{style, [<<\"width: \">>, arizona_template:local(<<\"w\">>, 100), "
        "            <<\"%\">>]}], [<<\"x\">>]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"style=\"width: 100%\"")),
    ?assertEqual(
        #{~"a" => #{~"style" => ~"w"}, ~"ap" => #{~"style" => [~"width: ", ~"%"]}},
        decode_local_descriptor(HTML)
    ).

%% Several static segments around the local concatenate into one prefix/suffix.
local_attr_interp_multi_static(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_attr_multistatic). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, [<<\"a \">>, <<\"b \">>, "
        "            arizona_template:local(<<\"c\">>, <<\"x\">>), <<\" y\">>, <<\" z\">>]}], "
        "            [<<\"t\">>]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"class=\"a b x y z\"")),
    ?assertEqual(
        #{~"a" => #{~"class" => ~"c"}, ~"ap" => #{~"class" => [~"a b ", ~" y z"]}},
        decode_local_descriptor(HTML)
    ).

%% ?local is HTML-only: in a ?native template it is a compile error (the client
%% slot mechanism -- comment markers + DOM set -- has no native equivalent).
local_in_native_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native("
        "        {'Text', [], [arizona_template:local(<<\"k\">>, <<\"x\">>)]}"
        "    ). ",
        fun(R) -> R =:= local_unsupported end
    ).

%% A content ?local inside a raw-text element (script/style/textarea/title)
%% renders markerless, so the client's az-local scan can never resolve the slot
%% and the value would silently never update. It must be a compile error.
local_content_in_raw_text_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_raw). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'script', [], [arizona_template:local(<<\"k\">>, <<\"x\">>)]}"
        "    ). ",
        fun(R) -> R =:= local_in_raw_text end
    ).

%% An *attribute* ?local on a raw-text element is fine (the attribute value is not
%% raw-text content) -- only the content slot is markerless, so it still compiles.
local_attr_in_raw_text_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_raw_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'script', [{'data-k', arizona_template:local(<<\"k\">>, <<\"v\">>)}], "
        "            [<<\"body\">>]}"
        "    ). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(#{~"a" => #{~"data-k" => ~"k"}}, decode_local_descriptor(HTML)).

%% ?local renders inside an ?each item. Keys are compile-time literals, so every
%% item carries the SAME slot key (shared) -- this pins that `?local` works in a
%% comprehension and each item gets its own descriptor + initial value.
local_in_each_renders(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [], [arizona_template:each(fun(Item) -> "
        "            {'li', [], ["
        "                arizona_template:local(<<\"m\">>, <<\"-\">>), "
        "                arizona_template:get(label, Item)]} "
        "        end, arizona_template:get(items, Bindings))]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{items => [#{label => ~"a"}, #{label => ~"b"}]}),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    %% Both items render the (shared-key) az-local descriptor + the initial "-".
    ?assertEqual(2, length(binary:matches(HTML, ~"az-local="))),
    ?assertEqual(2, length(binary:matches(HTML, ~"-<!--/az-->"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"a")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"b")).

%% An atom key is accepted and normalized to its binary form, both in the
%% rendered descriptor and the runtime bind-map.
local_atom_key(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_local_atom). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], [arizona_template:local(open, <<\"x\">>)]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{}),
    [{_Az, Fun, _Loc}] = maps:get(d, Tmpl),
    ?assertEqual(#{diff => false, az_local => ~"open", v => ~"x"}, Fun()),
    {HTML0, _Snap} = arizona_render:render(Tmpl),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(#{~"c" => #{~"0" => ~"open"}}, decode_local_descriptor(HTML)).

%% Normalization feeds the key-reuse guard: an atom key on an attribute and the
%% same key as a binary in content on one element still collide.
local_atom_binary_reuse_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_atom_reuse). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [{title, arizona_template:local(x, <<\"\">>)}], "
        "            [arizona_template:local(<<\"x\">>, <<\"y\">>)]}"
        "    ). ",
        fun(R) -> R =:= local_key_reused end
    ).

%% A ?local at the top level of a fragment has no enclosing element to carry
%% the az-local descriptor: the client could never bind the key, so
%% set/set_all would be silent no-ops. Compile error (before the fix it
%% rendered the init with no descriptor anywhere).
local_orphaned_fragment_top_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_orphan_frag). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html([{'span', [], [<<\"x\">>]}, "
        "        arizona_template:local(count, 0)]). ",
        fun(R) -> R =:= local_orphaned end
    ).

%% A ?local as a bare conditional-branch value never reaches the enclosing
%% element's descriptor scan (the child is the case expr, not the local), so
%% the key would be unreachable -- and a conditionally-present local makes no
%% sense against a static descriptor. Compile error.
local_orphaned_conditional_branch_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_orphan_cond). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(open, Bindings) of"
        "            true -> arizona_template:local(k, <<\"v\">>);"
        "            false -> <<>>"
        "        end"
        "    ]}). ",
        fun(R) -> R =:= local_orphaned end
    ).

%% A whole-template-body ?local (`?html(?local(...))`) is equally orphaned:
%% no element wraps the slot, so no az-local descriptor is emitted.
local_orphaned_whole_body_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_orphan_body). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(arizona_template:local(k, <<\"v\">>)). ",
        fun(R) -> R =:= local_orphaned end
    ).

%% Test 1: Static-only element -- no dynamics, single static binary.
static_only(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_static). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [{class, <<\"greeting\">>}], [<<\"Hello!\">>]}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<p class=\"greeting\">Hello!</p>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)),
    ?assert(is_binary(maps:get(f, T))).

%% Test 2: Single dynamic text child -- statics split, comment markers present.
single_dynamic_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_dyn_text). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [<<\"Hello, \">>, arizona_template:get(name, Bindings, <<\"world\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{name => <<"Alice">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<p az=\"0\">Hello, <!--az:0-->">>,
                <<"<!--/az--></p>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_dyn_text, 1}}] = maps:get(d, T),
    ?assertEqual(<<"Alice">>, Fun()).

%% Test 3: Single dynamic attribute -- statics split at attr value.
single_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_dyn_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, arizona_template:get(theme, Bindings, <<\"light\">>)}],"
        "         [<<\"content\">>]}"
        "    ). "
    ),
    T = Mod:render(#{theme => <<"dark">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<">content</div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {attr, <<"class">>, Fun}, {pt_dyn_attr, 1}}] = maps:get(d, T),
    ?assertEqual(<<"dark">>, Fun()).

%% Test 4: Counter pattern -- exact match with arizona_counter.erl.
counter_pattern(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_counter). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, arizona_template:get(id, Bindings, <<\"counter\">>)}], ["
        "            {'button', [{<<\"az-click\">>, <<\"inc\">>}], [<<\"+\">>]},"
        "            {'button', [{<<\"az-click\">>, <<\"dec\">>}], [<<\"-\">>]},"
        "            {'p', [], [<<\"Count: \">>, arizona_template:get(count, Bindings, 0)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{id => <<"test">>, count => 42}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<
                    ">"
                    "<button az-click=\"inc\">+</button>"
                    "<button az-click=\"dec\">-</button>"
                    "<p az=\"1\">Count: <!--az:1-->"
                >>,
                <<"<!--/az--></p></div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [
        {Az0, {attr, <<"id">>, IdFun}, {pt_counter, 1}},
        {Az1, {esc, CountFun}, {pt_counter, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"test">>, IdFun()),
    ?assertEqual(42, CountFun()).

%% Test 5: Nested elements -- recursive compilation, correct az assignment.
nested_elements(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            {'span', [], [arizona_template:get(a, Bindings, <<\"x\">>)]},"
        "            {'span', [], [arizona_template:get(b, Bindings, <<\"y\">>)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"hello">>, b => <<"world">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<
                    "<div>"
                    "<span az=\"0\"><!--az:0-->"
                >>,
                <<
                    "<!--/az--></span>"
                    "<span az=\"1\"><!--az:1-->"
                >>,
                <<
                    "<!--/az--></span>"
                    "</div>"
                >>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [{Az0, {esc, FunA}, {pt_nested, 1}}, {Az1, {esc, FunB}, {pt_nested, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, FunA()),
    ?assertEqual(<<"world">>, FunB()).

%% Test 6: Void elements -- self-closing, no children.
void_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_void). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            {'input', [{type, <<\"text\">>},"
        "             {value, arizona_template:get(val, Bindings, <<>>)}], []}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{val => <<"hello">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div><input az=\"0\" type=\"text\"">>,
                <<" /></div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {attr, <<"value">>, Fun}, {pt_void, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 7: Boolean attributes -- no value.
boolean_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bool). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'input', [{type, <<\"checkbox\">>}, checked], []}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<input type=\"checkbox\" checked />">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% Test 8: Multiple dynamics on same element (attr + content) -- shared az.
shared_az(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_shared). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, arizona_template:get(theme, Bindings, <<\"light\">>)}], ["
        "            arizona_template:get(content, Bindings, <<\"default\">>)"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{theme => <<"dark">>, content => <<"hello">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<"><!--az:0-->">>,
                <<"<!--/az--></div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [
        {Az0, {attr, <<"class">>, ThemeFun}, {pt_shared, 1}},
        {Az0, {esc, ContentFun}, {pt_shared, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"dark">>, ThemeFun()),
    ?assertEqual(<<"hello">>, ContentFun()).

%% A mixed fragment (a bare value interleaved with an element) must give the
%% bare value its OWN az and its own text-slot markers. Previously the bare
%% dynamic was hardcoded to a markerless az "0", which collided with the first
%% element's az -- and that element's first content-slot marker az is also "0"
%% (text_az(0, 0)) -- so an OP_TEXT for the bare value resolved to the element's
%% slot and overwrote it, while the bare value (having no marker) stayed stale.
mixed_fragment_bare_value_gets_own_az(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_mixed_frag). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"Hi \">>,"
        "        arizona_template:get(name, Bindings, <<\"Ada\">>),"
        "        {'b', [], [arizona_template:get(bold, Bindings, <<\"Lovelace\">>)]}"
        "    ]). "
    ),
    T = Mod:render(#{name => <<"Grace">>, bold => <<"Lovelace">>}),
    Fp = maps:get(f, T),
    %% The bare value is now wrapped in its own `<!--az:0-->...<!--/az-->` markers
    %% and the element is pushed to az "1".
    ?assertEqual(
        scope_s(Fp, [
            <<"Hi <!--az:0-->">>,
            <<"<!--/az--><b az=\"1\"><!--az:1-->">>,
            <<"<!--/az--></b>">>
        ]),
        maps:get(s, T)
    ),
    NameAz = <<Fp/binary, "-0">>,
    BoldAz = <<Fp/binary, "-1">>,
    %% Distinct az for the bare value vs the element's content slot -- the fix.
    ?assertNotEqual(NameAz, BoldAz),
    [
        {NameAz, {esc, NameFun}, {pt_mixed_frag, 1}},
        {BoldAz, {esc, BoldFun}, {pt_mixed_frag, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"Grace">>, NameFun()),
    ?assertEqual(<<"Lovelace">>, BoldFun()).

%% Test 9: Event attrs (az-click) -- fully static, NO az assigned.
event_attrs_static(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_event). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'button', [{<<\"az-click\">>, <<\"submit\">>}], [<<\"Click\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<button az-click=\"submit\">Click</button>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% Test 10: Empty children -- element with no content.
empty_children(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_empty). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [{id, <<\"empty\">>}], []}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<div id=\"empty\"></div>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% Test 11: Fingerprint stability -- same template, same fingerprint.
fingerprint_stability(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fp). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Bindings, <<\"a\">>)]}). "
    ),
    T1 = Mod:render(#{x => <<"a">>}),
    T2 = Mod:render(#{x => <<"b">>}),
    ?assertEqual(maps:get(f, T1), maps:get(f, T2)),
    ?assert(is_binary(maps:get(f, T1))).

%% The fingerprint keys the client's persistent statics cache, so two distinct
%% templates sharing a fingerprint silently render the wrong cached markup. The
%% statics [~"22906"] and [~"28755"] collide under phash2/1 (2^27) but not over the
%% full 2^32 range -- so these two static templates share a fingerprint before the
%% widening and must differ after it.
fingerprint_avoids_phash2_collision(Config) when is_list(Config) ->
    ModA = compile_module(
        "-module(pt_fp_collide_a). "
        "-export([render/1]). "
        "render(_Bindings) -> arizona_template:html(<<\"22906\">>). "
    ),
    ModB = compile_module(
        "-module(pt_fp_collide_b). "
        "-export([render/1]). "
        "render(_Bindings) -> arizona_template:html(<<\"28755\">>). "
    ),
    %% Same collision under the old default phash2/1 range, proving the statics really
    %% collide (the precondition that made the two fingerprints equal before the fix).
    ?assertEqual(erlang:phash2([~"22906"]), erlang:phash2([~"28755"])),
    FA = maps:get(f, ModA:render(#{})),
    FB = maps:get(f, ModB:render(#{})),
    ?assertNotEqual(FA, FB).

%% Test 12: Integration -- arizona:render/1 produces valid HTML.
render_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, arizona_template:get(id, Bindings, <<\"test\">>)}], ["
        "            <<\"Count: \">>, arizona_template:get(count, Bindings, 0)"
        "        ]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{id => <<"myview">>, count => 42}),
    Fp = maps:get(f, Tmpl),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    Expected = iolist_to_binary(
        scope_s(
            Fp,
            [<<"<div az=\"0\" id=\"myview\">Count: <!--az:0-->42<!--/az--></div>">>]
        )
    ),
    ?assertEqual(Expected, iolist_to_binary(HTML)).

%% Test 13: Integration -- arizona:diff/2 produces correct ops.
diff_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_diff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, arizona_template:get(cls, Bindings, <<\"a\">>)}], ["
        "            arizona_template:get(text, Bindings, <<\"hello\">>)"
        "        ]}"
        "    ). "
    ),
    %% First render
    T1 = Mod:render(#{cls => <<"a">>, text => <<"hello">>}),
    Fp = maps:get(f, T1),
    Az0 = <<Fp/binary, "-0">>,
    {_HTML, Snap} = arizona_render:render(T1),
    %% Update and diff
    T2 = Mod:render(#{cls => <<"b">>, text => <<"world">>}),
    {Ops, _NewSnap} = arizona_diff:diff(T2, Snap),
    %% Should have OP_SET_ATTR(1) for class and OP_TEXT(0) for text
    ?assertEqual(2, length(Ops)),
    ?assert(
        lists:any(
            fun
                ([1, Target, <<"class">>, <<"b">>]) -> Target =:= Az0;
                (_) -> false
            end,
            Ops
        )
    ),
    ?assert(
        lists:any(
            fun
                ([0, Target, <<"world">>]) -> Target =:= Az0;
                (_) -> false
            end,
            Ops
        )
    ).

%% =============================================================================
%% HTML auto-escaping
%% =============================================================================

%% A value interpolation in element content is HTML-escaped at SSR.
ssr_escapes_content_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_content). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Bindings)]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{x => <<"<script>alert(1)</script>">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, <<"<script>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"&lt;script&gt;">>)).

%% A value interpolation in an attribute is HTML-escaped (no quote breakout).
ssr_escapes_attr_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{title, arizona_template:get(t, Bindings)}], [<<\"x\">>]}"
        "    ). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{t => <<"\"><script>">>})),
    HTML = iolist_to_binary(HTML0),
    %% The closing-quote breakout must not survive verbatim.
    ?assertEqual(nomatch, binary:match(HTML, <<"\"><script>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"&quot;">>)).

%% All five HTML-significant characters map to entities.
ssr_escape_all_unsafe_chars(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_all). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Bindings)]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{x => <<"<>&\"'">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"&lt;&gt;&amp;&quot;&#39;">>)).

%% raw/1 opts a value out of escaping -- emitted verbatim.
raw_opt_out_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_raw). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        arizona_template:raw(arizona_template:get(x, Bindings))"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{x => <<"<b>raw</b>">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<b>raw</b>">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"&lt;b&gt;">>)).

%% The az facade's raw/1 behaves identically (the parse transform classifies
%% az:raw as a block, so the az:raw/1 runtime alias must exist and opt out).
az_raw_opt_out_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_az_raw). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html({'div', [], [az:raw(az:get(x, Bindings))]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{x => <<"<b>raw</b>">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<b>raw</b>">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"&lt;b&gt;">>)).

%% A nested ?html block is spliced structurally (its tags stay raw, not escaped).
block_value_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_block). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        arizona_template:html({'span', [], [arizona_template:get(x, Bindings)]})"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{x => <<"hi">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<span">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"&lt;span">>)).

%% A value expression that returns a nested ?html (the login error pattern):
%% the block renders structurally, yet a value interpolated inside is escaped.
value_template_inner_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_case_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(msg, Bindings) of "
        "            undefined -> <<\"\">>; "
        "            Msg -> arizona_template:html({'p', [], [Msg]}) "
        "        end "
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{msg => <<"<script>">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<p">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"<script>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"&lt;script&gt;">>)).

%% The live-diff path stays RAW: the OP_TEXT op carries the unescaped value
%% (the JS client sets textContent, which self-escapes -- escaping here would
%% double-display as &lt;b&gt;).
diff_value_stays_raw(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_diff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Bindings)]}). "
    ),
    T1 = Mod:render(#{x => <<"a">>}),
    {_HTML, Snap} = arizona_render:render(T1),
    T2 = Mod:render(#{x => <<"<b>">>}),
    {Ops, _NewSnap} = arizona_diff:diff(T2, Snap),
    ?assert(
        lists:any(
            fun
                ([0, _Target, V]) -> V =:= <<"<b>">>;
                (_) -> false
            end,
            Ops
        )
    ).

%% The fingerprint (initial hydration) wire payload escapes value dynamics.
fingerprint_escapes_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_fp). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Bindings)]}). "
    ),
    {_HTML, Snap} = arizona_render:render(Mod:render(#{x => <<"<i>">>})),
    Payload = arizona_render:fingerprint_payload(Snap),
    DBin = iolist_to_binary(maps:get(~"d", Payload)),
    ?assertEqual(nomatch, binary:match(DBin, <<"<i>">>)),
    ?assertNotEqual(nomatch, binary:match(DBin, <<"&lt;i&gt;">>)).

%% Escaping is governed by the backend module, not the render target. The native
%% backend marks value interpolations like every backend (`{esc, Fun}`), but its
%% `arizona_renderer:escape/1` is the identity, so values reach the JSON wire
%% unescaped (HTML-escaping there would corrupt the output).
native_backend_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native({'Text', [], [arizona_template:get(x, Bindings)]}). "
    ),
    T = Mod:render(#{x => <<"<not-escaped>">>}),
    ?assertEqual(arizona_native, maps:get(backend, T)),
    ?assertMatch([{_Az, {esc, _Fun}, _Loc}], maps:get(d, T)),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    %% Never HTML-escaped: the zipped document carries it as a JSON string, the
    %% wire carries it raw for the client to JSON-encode.
    ?assertEqual(<<"\"<not-escaped>\"">>, arizona_template:escape_value(arizona_native, Fun())),
    ?assertEqual(<<"<not-escaped>">>, arizona_native:wire_escape(Fun())).

%% A bare ?get inside <script> is spliced VERBATIM (raw text decodes no character
%% references, so escaping cannot apply) -- the value could close the JS string it
%% sits in and inject code. Every other slot kind escapes, so this is the one
%% unmarked-value-reaches-output hole: it must be a compile error, not silent XSS.
raw_text_script_bare_dynamic_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_rt_bare_script). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        <<\"var user = \\\"\">>, arizona_template:get(v, Bindings), <<\"\\\";\">>"
        "    ]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

%% Same rule for <style>: CSS is raw text too, so an unmarked value reaches the
%% output verbatim and can close the element.
raw_text_style_bare_dynamic_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_rt_bare_style). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'style', [], [arizona_template:get(css, Bindings)]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

%% The opt-out must be literal at the slot, mirroring how the transform already
%% recognizes ?raw everywhere else. A conditional that yields values is not a
%% ?raw call, so it is rejected -- wrap the whole conditional instead.
raw_text_conditional_dynamic_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_rt_bare_cond). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        case arizona_template:get(env, Bindings) of "
        "            prod -> <<\"prod()\">>; "
        "            _ -> <<\"dev()\">> "
        "        end"
        "    ]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

%% A value computed outside the template (no binding read, so not inlined back
%% into the slot) is the classic shape that smuggles an unmarked value into a
%% script. It is rejected too -- ?raw is what states "already safe here".
raw_text_helper_call_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_rt_bare_call). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Json = erlang:atom_to_binary(ok), "
        "    arizona_template:html({'script', [], [Json]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

%% The common case -- a <script>/<style> whose children are only literal JS/CSS
%% text -- carries no dynamic slot at all and must stay untouched by the rule.
raw_text_static_only_still_compiles(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_static_only). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'script', [], [<<\"var a = 1 < 2 && 3 > 2;\">>]}, "
        "        {'style', [], [<<\".a > .b { color: red }\">>]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(
        <<
            "<div><script>var a = 1 < 2 && 3 > 2;</script>"
            "<style>.a > .b { color: red }</style></div>"
        >>,
        HTML
    ).

%% The `az:` alias of the opt-out is recognized exactly like arizona_template:raw.
raw_text_az_alias_raw_accepted(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_az_alias). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], [az:raw(az:get(js, Bindings))]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{js => ~"a && b"})),
    ?assertEqual(<<"<script az=\"11AQ3M-0\">a && b</script>">>, iolist_to_binary(HTML0)).

%% The rejection reason renders through format_error/1 (what the compiler prints),
%% and names the ?raw opt-out that unblocks the author.
raw_text_reject_message_renders(Config) when is_list(Config) ->
    Msg = arizona_parse_transform:format_error(dynamic_in_raw_text),
    ?assert(is_list(Msg)),
    ?assertNotEqual(nomatch, string:find(Msg, "?raw")),
    ?assertNotEqual(nomatch, string:find(Msg, "script")).

%% az-nodiff must not be an escape hatch out of the raw-text rule. The nodiff
%% clause of emit_child_dynamic/4 once matched first, so an az-nodiff element
%% (layouts, the very place inline <script> lives) skipped the opt-out guard
%% entirely and HTML-escaped the value inside raw text -- broken JS, and the
%% author got no compile signal at all.
raw_text_in_nodiff_bare_dynamic_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_rt_nodiff_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', ['az-nodiff'], ["
        "        {'script', [], [arizona_template:get(v, Bindings)]}"
        "    ]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

%% ...and the marked value in an az-nodiff element must still reach raw_text/2.
%% Skipping the raw-text clause sent it through the nodiff path, which neutralizes
%% nothing, so a ?raw close tag broke straight out of the element.
raw_text_in_nodiff_raw_neutralized(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_nodiff_raw). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', ['az-nodiff'], ["
        "        {'script', [], ["
        "            arizona_template:raw(arizona_template:get(v, Bindings))"
        "        ]}"
        "    ]}). "
    ),
    Hostile = ~"</script><img src=x onerror=alert(1)>",
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{v => Hostile})),
    HTML = iolist_to_binary(HTML0),
    %% The breakout is defused: only the element's own close tag survives.
    ?assertEqual(nomatch, binary:match(HTML, ~"</script><img")),
    ?assertEqual(1, length(binary:matches(HTML, ~"</script>"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<\\/script><img")),
    %% Still verbatim (not HTML-escaped) and still markerless, as raw text requires.
    ?assertEqual(nomatch, binary:match(HTML, ~"&lt;")),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")).

%% <style> is RAWTEXT: it has no script-data-escaped state, so `<!--`/`<script`
%% neutralization protects nothing there and only corrupts the CSS -- `<` is
%% a JS/JSON escape, and a CSS parser reads it as the identifier bytes `u003c`.
%% (`<!--`/`-->` are legitimate CSS tokens, CDO/CDC.) A style slot must therefore
%% round-trip byte for byte.
raw_text_style_css_round_trip(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_style_css). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'style', [], ["
        "        arizona_template:raw(arizona_template:get(css, Bindings))"
        "    ]}). "
    ),
    Css = ~"<!-- .a > .b { color: red } --> .c:before { content: \"<script>\" }",
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{css => Css})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(<<"<style az=\"1PLA63N-0\">", Css/binary, "</style>">>, HTML),
    %% Specifically: no JS escape leaked into the stylesheet.
    ?assertEqual(nomatch, binary:match(HTML, ~"u003c")).

%% ...but the one sequence that DOES end a RAWTEXT element is still neutralized in
%% <style>, and <script> keeps the full script-data treatment. The tag decides the
%% policy; sharing one blanket policy is what corrupted the CSS above.
raw_text_style_close_tag_still_neutralized(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_style_close). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'style', [], [arizona_template:raw(arizona_template:get(css, Bindings))]}, "
        "        {'script', [], [arizona_template:raw(arizona_template:get(js, Bindings))]}"
        "    ]}). "
    ),
    Breakout = ~"</style><img src=x onerror=alert(1)>",
    Double = ~"<!--<script>",
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{css => Breakout, js => Double})),
    HTML = iolist_to_binary(HTML0),
    %% style: the close tag is broken, so only the element's own survives.
    ?assertEqual(nomatch, binary:match(HTML, ~"</style><img")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<\\/style><img")),
    ?assertEqual(1, length(binary:matches(HTML, ~"</style>"))),
    %% script: both halves of the double escape are still rewritten.
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"\\u003c!--\\u003cscript>")),
    ?assertEqual(1, length(binary:matches(HTML, ~"<script"))).

%% Where a hoisted ?raw is accepted is decided by the binding-read inlining, which
%% splices a variable back into its slot only when its expression reads a binding.
%% So `R = ?raw(?get(js))` reaches the slot as a literal ?raw and compiles, while
%% `R = ?raw(<<"x">>)` never does and is rejected. Pin both sides so the accepted
%% one is a documented boundary rather than an accident, and require the error to
%% name the remedy (wrap at the slot) instead of leaving the author to guess.
raw_text_hoisted_raw_boundary(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_hoist_read). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    R = arizona_template:raw(arizona_template:get(js, Bindings)), "
        "    arizona_template:html({'script', [], [R]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{js => ~"a && b"})),
    ?assertEqual(<<"<script az=\"11AQ3M-0\">a && b</script>">>, iolist_to_binary(HTML0)),
    %% No binding read in the hoisted expression -- not inlined, so not a literal
    %% ?raw at the slot, so rejected.
    assert_parse_error(
        "-module(pt_rt_hoist_noread). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    R = arizona_template:raw(<<\"x\">>), "
        "    arizona_template:html({'script', [], [R]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ),
    %% The message must point at the fix that always works: wrap AT the slot.
    Msg = arizona_parse_transform:format_error(dynamic_in_raw_text),
    ?assertNotEqual(nomatch, string:find(Msg, "at the slot")),
    ?assertNotEqual(nomatch, string:find(Msg, "variable")).

%% HTML tag names are ASCII case-insensitive, so `<SCRIPT>` IS a script element to
%% the browser. Classifying only the lowercase atom left an uppercase tag treated
%% as an ordinary element: comment markers landed in the script (a SyntaxError for
%% a module script) and the guard never fired.
raw_text_uppercase_tag_classified(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_rt_upper_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'SCRIPT', [], [arizona_template:get(v, Bindings)]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ),
    Mod = compile_module(
        "-module(pt_rt_upper_raw). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ScRiPt', [], ["
        "        arizona_template:raw(arizona_template:get(v, Bindings))"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{v => ~"a</script>b"})),
    HTML = iolist_to_binary(HTML0),
    %% Markerless, verbatim, and the script-data policy applies (the tag is matched
    %% case-insensitively on the way into the neutralizer too).
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<ScRiPt az=\"1KL9229-0\">a<\\/script>b</ScRiPt>">>, HTML),
    %% The classifier itself, for every raw-text tag and a non-raw-text one.
    ?assertEqual(raw, arizona_html:raw_text_kind('SCRIPT', html)),
    ?assertEqual(raw, arizona_html:raw_text_kind('Style', html)),
    ?assertEqual(escapable, arizona_html:raw_text_kind('TEXTAREA', html)),
    ?assertEqual(escapable, arizona_html:raw_text_kind('Title', html)),
    ?assertEqual(none, arizona_html:raw_text_kind('DIV', html)).

%% Inside `<svg>` the parser is in foreign content, where an element is ordinary
%% parsed markup: comments really are comments, so a slot keeps its markers and
%% stays diffable. `title` is the tag that differs -- HTML's makes markers literal
%% text, SVG's does not -- and an SVG `<title>` is a graphic's accessible name, so
%% classifying it as escapable raw text froze a bound one after SSR.
%% `<foreignObject>` switches back, so its `<title>` keeps the HTML treatment.
raw_text_svg_title_diffable(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_svg_title). "
        "-export([svg/1, fo/1]). "
        "svg(Bindings) -> "
        "    arizona_template:html({svg, [], [{title, [], "
        "        [arizona_template:get(t, Bindings)]}]}). "
        "fo(Bindings) -> "
        "    arizona_template:html({svg, [], [{foreignObject, [], [{title, [], "
        "        [arizona_template:get(t, Bindings)]}]}]}). "
    ),
    %% SVG: marked, and a changed value produces an op.
    {SvgHTML0, SvgSnap} = arizona_render:render(Mod:svg(#{t => ~"A"})),
    SvgHTML = iolist_to_binary(SvgHTML0),
    ?assertMatch({_, _}, binary:match(SvgHTML, ~"<!--az:")),
    {SvgOps, _} = arizona_diff:diff(Mod:svg(#{t => ~"B"}), SvgSnap),
    ?assertMatch([[_Op, _Az, ~"B"]], SvgOps),
    %% foreignObject: back to HTML rules -- markerless and render-once.
    {FoHTML0, FoSnap} = arizona_render:render(Mod:fo(#{t => ~"A"})),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(FoHTML0), ~"<!--az:")),
    %% `<foreignObject>` switches back to html, so `title` IS raw text there and its
    %% slot stays markerless -- but the element carries an az, so the value still
    %% reaches the DOM as a whole-content re-render rather than freezing.
    ?assertMatch(
        [[_Op, <<"1BEHNP5-0">>, #{~"d" := [~"B"]}]],
        element(1, arizona_diff:diff(Mod:fo(#{t => ~"B"}), FoSnap))
    ),
    %% The classifier itself, in both contexts.
    ?assertEqual(none, arizona_html:raw_text_kind('Title', foreign)),
    %% `style`/`script` are raw text ONLY in HTML. Inside `<svg>` their content is
    %% ordinary parsed markup (a comment is a comment, a bare `<` starts an
    %% element), so classifying them `raw` there both froze the slot and forced a
    %% verbatim `?raw` splice into a context that parses markup.
    ?assertEqual(none, arizona_html:raw_text_kind('Style', foreign)),
    ?assertEqual(none, arizona_html:raw_text_kind('Script', foreign)),
    ?assertEqual(raw, arizona_html:raw_text_kind('Style', html)),
    ?assertEqual(raw, arizona_html:raw_text_kind('Script', html)),
    ?assertEqual(foreign, arizona_html:content_context('SVG', html)),
    ?assertEqual(html, arizona_html:content_context('FOREIGNOBJECT', foreign)),
    ?assertEqual(foreign, arizona_html:content_context(g, foreign)).

%% The content context is per-template, so how SVG chrome is factored decides
%% whether its `<title>` keeps updating. An element helper is inlined into the
%% caller's compile and carries the context; a `?stateless` child compiles with
%% its own `html` context and stays render-once, since it cannot know its call
%% site. Pinned because the difference is invisible in the markup -- both spell
%% the same `<title>` -- and the component form fails silently.
raw_text_svg_title_factoring(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_svg_factor). "
        "-export([helper/1, component/1, icon/1, each/1]). "
        "helper(Bindings) -> "
        "    arizona_template:html({svg, [], [badge(arizona_template:get(t, Bindings))]}). "
        "badge(T) -> {title, [], [T]}. "
        "component(Bindings) -> "
        "    arizona_template:html({svg, [], [arizona_template:stateless(fun icon/1, "
        "        #{t => arizona_template:get(t, Bindings)})]}). "
        "icon(Bindings) -> "
        "    arizona_template:html({title, [], [arizona_template:get(t, Bindings)]}). "
        "each(Bindings) -> "
        "    arizona_template:html({svg, [], [arizona_template:each("
        "        fun(R) -> {g, [], [{title, [], [R]}]} end, "
        "        arizona_template:get(rows, Bindings))]}). "
    ),
    %% Helper: spliced into the caller, so the `<svg>` context reaches it.
    {HelperHTML, HelperSnap} = arizona_render:render(Mod:helper(#{t => ~"A"})),
    ?assertMatch({_, _}, binary:match(iolist_to_binary(HelperHTML), ~"<!--az:")),
    ?assertMatch(
        [[_Op, _Az, ~"B"]],
        element(1, arizona_diff:diff(Mod:helper(#{t => ~"B"}), HelperSnap))
    ),
    %% Component: its own template, its own `html` context, so it classifies `title`
    %% as escapable raw text and the slot inside it comes out markerless. The
    %% component's OWN slot in the caller still has markers, so the change escalates
    %% to a whole re-render of that nested template instead of being dropped.
    {_CompHTML, CompSnap} = arizona_render:render(Mod:component(#{t => ~"A"})),
    %% The `<title>` inside the child now carries its own az and its content folds
    %% into one nested template, so the patch is scoped to that element rather than
    %% re-rendering the whole child.
    ?assertMatch(
        [[_Op, _Az, #{~"s" := [<<>>, <<>>], ~"d" := [~"B"]}]],
        element(1, arizona_diff:diff(Mod:component(#{t => ~"B"}), CompSnap))
    ),
    %% `?each` DOES carry it: the top-down pass marks the each with the backend and
    %% the enclosing context, and the per-item template compiles in that context.
    %% Pinned against the component case above, which cannot.
    {EachHTML, _} = arizona_render:render(Mod:each(#{rows => [~"A"]})),
    ?assertMatch({_, _}, binary:match(iolist_to_binary(EachHTML), ~"<title az=")).

%% A content `?local` under a raw-text element is a compile error -- the client
%% resolves a local slot through its markers, and raw-text content has none. An
%% SVG `<title>` is not raw text, so it carries markers and a `?local` there is
%% legal and resolvable. Pinned because the rule now depends on the content
%% context, not the tag alone.
raw_text_svg_title_local(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_svg_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({svg, [], [{title, [], "
        "        [arizona_template:local(~\"t\", ~\"A\")]}]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{}))),
    ?assertMatch({_, _}, binary:match(HTML, ~"az-local=")),
    ?assertMatch({_, _}, binary:match(HTML, ~"<!--az:")),
    %% The HTML `<title>` form stays rejected.
    assert_parse_error(
        "-module(pt_rt_html_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'head', [], [{title, [], "
        "        [arizona_template:local(~\"t\", ~\"A\")]}]}). ",
        fun(R) -> R =:= local_in_raw_text end
    ).

%% A dynamic inside `<svg><style>` needs no `?raw` and is ESCAPED. In foreign
%% content the parser treats script/style as ordinary markup, so it decodes the
%% references straight back (`a &lt; b` reaches CSS as `a < b`) while a bare `<`
%% in the value can no longer start an element. The HTML form still demands
%% `?raw`, because there the browser decodes nothing and escaping would corrupt.
raw_text_foreign_script_style_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_foreign_style). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({svg, [], [{style, [], "
        "        [arizona_template:get(v, Bindings)]}]}). "
    ),
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:render(#{v => ~"a<img src=x>b"}))
    ),
    %% Escaped, so the value cannot start an element.
    ?assertEqual(nomatch, binary:match(HTML, ~"<img")),
    ?assertMatch({_, _}, binary:match(HTML, ~"a&lt;img src=x&gt;b")),
    %% Marker-anchored, so the slot is diffable rather than render-once.
    ?assertMatch({_, _}, binary:match(HTML, ~"<!--az:")),
    %% The HTML form is unchanged: still a compile error without ?raw.
    assert_parse_error(
        "-module(pt_rt_html_style). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [{style, [], "
        "        [arizona_template:get(v, Bindings)]}]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

%% A dynamic content slot inside <script> renders WITHOUT comment markers: the
%% browser would treat <!--az:...--> as literal script bytes (a module script's
%% HTML-comment tokens are a SyntaxError), so the slot must be markerless.
raw_text_script_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_script). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [{type, <<\"module\">>}], ["
        "        arizona_template:raw(arizona_template:get(boot, Bindings))"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{boot => <<"import x from \"/c\"">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--/az-->")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"import x from \"/c\"")),
    ?assertEqual(<<"<script az=\"1C9RHX-0\" type=\"module\">import x from \"/c\"</script>">>, HTML).

%% A scalar inside <script> is emitted verbatim (NOT HTML-escaped): the parser
%% never decodes character references in raw text, so escaping `&`/`<` would
%% corrupt the JavaScript (`b=1&c=2` must not become `b=1&amp;c=2`).
raw_text_script_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_script_esc). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        arizona_template:raw(arizona_template:get(js, Bindings))"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{js => <<"a < b && c > d">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"a < b && c > d")),
    ?assertEqual(nomatch, binary:match(HTML, ~"&lt;")),
    ?assertEqual(nomatch, binary:match(HTML, ~"&amp;")).

%% A dynamic content slot inside <style> is markerless (markers would be invalid
%% CSS) and verbatim.
raw_text_style_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_style). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'style', [], ["
        "        arizona_template:raw(arizona_template:get(css, Bindings))"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{css => <<".a > .b { color: red }">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~".a > .b { color: red }")),
    ?assertEqual(nomatch, binary:match(HTML, ~"&gt;")).

%% The real JSON-LD use case: a ?raw value as <script type="application/ld+json">
%% content renders verbatim and markerless, so crawlers get valid JSON.
raw_text_json_ld_raw_verbatim(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_jsonld). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'script', [{type, <<\"application/ld+json\">>}], ["
        "            arizona_template:raw(arizona_template:get(jsonld, Bindings))"
        "        ]}"
        "    ). "
    ),
    JsonLd = <<"[{\"@type\":\"Product\",\"name\":\"x & y\"}]">>,
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{jsonld => JsonLd})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, JsonLd)),
    ?assertEqual(nomatch, binary:match(HTML, ~"&quot;")).

%% <title> is escapable raw text: comment markers would still be literal text, so
%% the slot is markerless, but a scalar IS HTML-escaped (the parser decodes
%% character references there, so `&`/`<` must be escaped to render correctly).
raw_text_title_escaped_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_title). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'title', [], [arizona_template:get(t, Bindings)]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{t => <<"A & B < C">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"&amp;")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"&lt;")).

%% <textarea> is also escapable raw text: markerless slot.
raw_text_textarea_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_textarea). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'textarea', [], [arizona_template:get(v, Bindings)]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{v => <<"hello">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"hello")).

%% Render-once: the raw-text slot is non-diffable (Az = undefined). Even when its
%% binding changes between renders, the diff emits no op -- there is no comment
%% marker for the client to patch. Covers both the bare diff/2 (child-view path)
%% and the production deps-aware diff/4, where the slot's binding is in `Changed`.
%% Patching an EXECUTABLE script is the one raw-text case where updating is
%% questionable: the browser does not re-run a script whose text changes in place
%% (its "already started" flag is set), so the element's source would say one thing
%% while the code in effect says another. `az-nodiff` is the author-facing opt-out
%% and the fold honours it -- no az, no op, render-once exactly as before.
raw_text_nodiff_keeps_script_render_once(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', ['az-nodiff'], ["
        "        arizona_template:raw(arizona_template:get(boot, Bindings))"
        "    ]}). "
    ),
    T1 = Mod:render(#{boot => <<"a">>}),
    {HTML0, Snap0} = arizona_render:render(T1),
    HTML = iolist_to_binary(HTML0),
    %% No az on the element and no markers inside it.
    ?assertEqual(<<"<script>a</script>">>, strip_nodiff_attr(HTML)),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    T2 = Mod:render(#{boot => <<"b">>}),
    ?assertEqual([], element(1, arizona_diff:diff(T2, Snap0))),
    {_Ops0, DepsSnap, _V0} = arizona_diff:diff(T1, Snap0, #{}),
    {Ops4, _S4, _V1} = arizona_diff:diff(T2, DepsSnap, #{}, #{boot => true}),
    ?assertEqual([], Ops4).

raw_text_content_diffs_against_element_az(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_once). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        arizona_template:raw(arizona_template:get(boot, Bindings))"
        "    ]}). "
    ),
    T1 = Mod:render(#{boot => <<"a">>}),
    {_HTML, Snap0} = arizona_render:render(T1),
    T2 = Mod:render(#{boot => <<"b">>}),
    {Ops, _Snap2} = arizona_diff:diff(T2, Snap0),
    ?assertMatch([[_Op, <<"11AQ3M-0">>, #{~"d" := [~"b"]}]], Ops),
    %% Production path: build a deps-carrying snapshot (as the live process does on
    %% mount), then diff/4 with the slot's binding explicitly marked changed.
    {_Ops0, DepsSnap, _V0} = arizona_diff:diff(T1, Snap0, #{}),
    {Ops4, _Snap4, _V1} = arizona_diff:diff(T2, DepsSnap, #{}, #{boot => true}),
    ?assertMatch([[_Op4, <<"11AQ3M-0">>, #{~"d" := [~"b"]}]], Ops4).

%% A dynamic *attribute* on a raw-text element stays fully diffable -- attribute
%% values are not raw-text content, so they keep the element az and emit an op
%% when changed. Only the element's content slot is markerless.
raw_text_dynamic_attr_still_diffable(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'script', [{src, arizona_template:get(src, Bindings)}], []}"
        "    ). "
    ),
    T1 = Mod:render(#{src => <<"/a.js">>}),
    {HTML0, Snap} = arizona_render:render(T1),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~" az=\"")),
    T2 = Mod:render(#{src => <<"/b.js">>}),
    {Ops, _Snap2} = arizona_diff:diff(T2, Snap),
    ?assertNotEqual([], Ops).

%% Regression guard: a content slot in an ORDINARY element still gets its
%% comment-marker diff target.
normal_element_keeps_markers(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_normal). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'span', [], [arizona_template:get(x, Bindings)]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{x => <<"hi">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<!--/az-->")).

%% Sibling restore: a normal element FOLLOWING a raw-text sibling must still get
%% its markers -- the raw-text context is scoped to the script's own children and
%% restored before the span compiles. Guards the `raw_text_kind` save/restore.
raw_text_sibling_after_restores_markers(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_sib_after). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'script', [], ["
        "            arizona_template:raw(arizona_template:get(boot, Bindings))"
        "        ]}, "
        "        {'span', [], [arizona_template:get(x, Bindings)]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{boot => <<"BOOT">>, x => <<"XV">>})),
    HTML = iolist_to_binary(HTML0),
    %% The script content is markerless; the span (after it) keeps its markers.
    ?assertNotEqual(nomatch, binary:match(HTML, ~">BOOT</script>")),
    ?assertEqual(1, length(binary:matches(HTML, ~"<!--az:"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"-->XV<!--/az-->")).

%% Sibling restore (reverse order): a normal element BEFORE a raw-text sibling is
%% unaffected -- it keeps markers, the trailing script is markerless.
raw_text_sibling_before_keeps_markers(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_sib_before). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'span', [], [arizona_template:get(x, Bindings)]}, "
        "        {'script', [], ["
        "            arizona_template:raw(arizona_template:get(boot, Bindings))"
        "        ]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{boot => <<"BOOT">>, x => <<"XV">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(1, length(binary:matches(HTML, ~"<!--az:"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"-->XV<!--/az-->")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~">BOOT</script>")).

%% A raw-text element BETWEEN two normal elements: both neighbours get markers
%% (two marked slots), the script in the middle is markerless, and the az indices
%% stay contiguous (the script consumes none).
raw_text_between_normal_siblings_markers(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_between). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'span', [], [arizona_template:get(a, Bindings)]}, "
        "        {'script', [], ["
        "            arizona_template:raw(arizona_template:get(boot, Bindings))"
        "        ]}, "
        "        {'p', [], [arizona_template:get(b, Bindings)]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(
        Mod:render(#{a => <<"AV">>, boot => <<"BOOT">>, b => <<"BV">>})
    ),
    HTML = iolist_to_binary(HTML0),
    %% Exactly two marked slots (span + p), none for the middle script.
    ?assertEqual(2, length(binary:matches(HTML, ~"<!--az:"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~">BOOT</script>")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"-->AV<!--/az-->")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"-->BV<!--/az-->")).

%% Az-numbering integrity: a raw-text slot and a normal diffable slot coexist. The
%% normal span still diffs correctly (emits an op when its binding changes) despite
%% the script consuming no az index; the script never emits an op. Covers diff/2
%% (child-view) and the production deps-aware diff/4.
raw_text_mixed_az_numbering_diffs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'script', [], ["
        "            arizona_template:raw(arizona_template:get(boot, Bindings))"
        "        ]}, "
        "        {'span', [], [arizona_template:get(count, Bindings)]}"
        "    ]}). "
    ),
    T1 = Mod:render(#{boot => <<"a">>, count => <<"1">>}),
    {_HTML, Snap0} = arizona_render:render(T1),
    %% diff/2: changing the span binding emits an op; changing only the script does not.
    {OpsCount, _} = arizona_diff:diff(Mod:render(#{boot => <<"a">>, count => <<"2">>}), Snap0),
    ?assertNotEqual([], OpsCount),
    %% The raw-text slot diffs too now, addressed by its element's az.
    {OpsBoot, _} = arizona_diff:diff(Mod:render(#{boot => <<"b">>, count => <<"1">>}), Snap0),
    ?assertNotEqual([], OpsBoot),
    %% diff/4 (production): same, keyed by the dirty binding set.
    {_Ops0, DepsSnap, _V0} = arizona_diff:diff(T1, Snap0, #{}),
    {OpsCount4, _, _} = arizona_diff:diff(
        Mod:render(#{boot => <<"a">>, count => <<"2">>}), DepsSnap, #{}, #{count => true}
    ),
    ?assertNotEqual([], OpsCount4),
    {OpsBoot4, _, _} = arizona_diff:diff(
        Mod:render(#{boot => <<"b">>, count => <<"1">>}), DepsSnap, #{}, #{boot => true}
    ),
    %% Dep-aware too: the fold injects track/1 touches, so the slot is not skipped.
    ?assertNotEqual([], OpsBoot4).

%% Two dynamic content slots in one raw-text element: both render verbatim, both
%% markerless (the whole element's content is raw text).
raw_text_two_content_slots(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_two). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        arizona_template:raw(arizona_template:get(a, Bindings)), "
        "        arizona_template:raw(arizona_template:get(b, Bindings))"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{a => <<"AA">>, b => <<"BB">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<script az=\"11AQ3M-0\">AABB</script>">>, HTML).

%% Static text around a dynamic slot inside a raw-text element survives in order,
%% markerless -- the computed value is spliced between the literal script bytes.
raw_text_static_text_around_slot(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_static). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        <<\"var x=\">>, "
        "        arizona_template:raw(arizona_template:get(url, Bindings)), "
        "        <<\"; init();\">>"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{url => <<"\"/u\"">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<script az=\"11AQ3M-0\">var x=\"/u\"; init();</script>">>, HTML).

%% A control-flow value (case) in a raw-text slot flows through
%% expand_block_element_tails: the selected branch renders verbatim and markerless,
%% and diffing is a no-op even when the scrutinee binding changes (render-once).
%% The conditional is wrapped in ?raw -- the opt-out marks the whole slot, so
%% control flow inside it is unaffected by the raw-text opt-out requirement.
raw_text_conditional_value_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_cond). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        arizona_template:raw("
        "            case arizona_template:get(env, Bindings) of "
        "                prod -> <<\"prod()\">>; "
        "                _ -> <<\"dev()\">> "
        "            end)"
        "    ]}). "
    ),
    T1 = Mod:render(#{env => prod}),
    {HTML0, Snap0} = arizona_render:render(T1),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<script az=\"11AQ3M-0\">prod()</script>">>, HTML),
    %% The SLOT carries no markers (asserted above), but the ELEMENT carries an az,
    %% so switching the scrutinee re-renders the element's content against it.
    {Ops, _} = arizona_diff:diff(Mod:render(#{env => dev}), Snap0),
    ?assertMatch([[_Op, <<"11AQ3M-0">>, #{~"d" := [~"dev()"]}]], Ops).

%% A raw-text element with BOTH a dynamic attribute and dynamic content: the
%% attribute stays diffable (the element keeps its az and the attr emits an op when
%% changed), but the content is markerless and never emits an op when changed.
raw_text_attr_diffs_content_render_once(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_attr_content). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', "
        "        [{src, arizona_template:get(src, Bindings)}], "
        "        [arizona_template:raw(arizona_template:get(body, Bindings))]}). "
    ),
    T1 = Mod:render(#{src => <<"/a.js">>, body => <<"A">>}),
    {HTML0, Snap0} = arizona_render:render(T1),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~" az=\"")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"src=\"/a.js\"")),
    %% The body is markerless content even though the element has an az for the attr.
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    %% Attribute and content are addressed separately: the attribute by its own
    %% ?OP_SET_ATTR, the content by an ?OP_TEXT against the element's az.
    {OpsSrc, _} = arizona_diff:diff(Mod:render(#{src => <<"/b.js">>, body => <<"A">>}), Snap0),
    ?assertNotEqual([], OpsSrc),
    {OpsBody, _} = arizona_diff:diff(Mod:render(#{src => <<"/a.js">>, body => <<"B">>}), Snap0),
    ?assertMatch([[_Op, <<"QP3ZUD-0">>, #{~"d" := [~"B"]}]], OpsBody).

%% Escapable (<title>) and raw (<script>) raw-text elements adjacent under <head>,
%% followed by a normal element: both kinds restore context, so the trailing
%% element still gets markers while both raw-text slots stay markerless.
raw_text_escapable_adjacent_to_raw(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_head). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'head', [], ["
        "        {'title', [], [arizona_template:get(t, Bindings)]}, "
        "        {'script', [], ["
        "            arizona_template:raw(arizona_template:get(boot, Bindings))"
        "        ]}, "
        "        {'meta', [{name, arizona_template:get(m, Bindings)}]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(
        Mod:render(#{t => <<"Hi">>, boot => <<"go()">>, m => <<"v">>})
    ),
    HTML = iolist_to_binary(HTML0),
    %% Neither raw-text slot is marked.
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~">Hi</title>")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~">go()</script>")),
    %% The trailing <meta> still carries its az (its dynamic attr is diffable).
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<meta az=")).

%% The raw-text rule is HTML-only: a `script`-tagged element on the native backend
%% is unaffected (raw_text_kind is `none`), so its content keeps a normal addressed
%% dynamic (a binary az), not the markerless `undefined`.
native_script_tag_unaffected(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native({'script', [], [arizona_template:get(x, Bindings)]}). "
    ),
    T = Mod:render(#{x => <<"NV">>}),
    ?assertMatch([{Az, _Fun, _Loc}] when is_binary(Az), maps:get(d, T)).

%% A bare element tuple as a `case` branch result is compiled into a nested
%% template (no ?html wrap needed): the block renders structurally, a value
%% interpolated inside is still escaped, and the empty branch renders nothing.
cond_case_bare_element_renders(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_case_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(msg, Bindings) of "
        "            undefined -> <<\"\">>; "
        "            Msg -> {'p', [{class, <<\"err\">>}], [Msg]} "
        "        end "
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{msg => <<"<script>">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<p">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"<script>">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"&lt;script&gt;">>)),
    {Empty0, _} = arizona_render:render(Mod:render(#{msg => undefined})),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Empty0), <<"<p">>)).

%% The bare form is byte-for-byte equivalent to the explicit ?html form: both
%% branch tails compile to the same nested template.
cond_case_bare_matches_html(Config) when is_list(Config) ->
    Bare = compile_module(
        "-module(pt_cond_bare_eq). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(msg, Bindings) of "
        "            undefined -> <<\"\">>; "
        "            Msg -> {'p', [{class, <<\"err\">>}], [Msg]} "
        "        end "
        "    ]}). "
    ),
    Wrap = compile_module(
        "-module(pt_cond_html_eq). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(msg, Bindings) of "
        "            undefined -> <<\"\">>; "
        "            Msg -> arizona_template:html({'p', [{class, <<\"err\">>}], [Msg]}) "
        "        end "
        "    ]}). "
    ),
    B = #{msg => <<"boom">>},
    {BareHTML, _} = arizona_render:render(Bare:render(B)),
    {WrapHTML, _} = arizona_render:render(Wrap:render(B)),
    ?assertEqual(iolist_to_binary(WrapHTML), iolist_to_binary(BareHTML)).

%% An `if` branch returning a bare element is expanded the same way (this guards the
%% `if` clause of map_tail_exprs). The condition reads a tracked binding (`?get(show)`):
%% the read can't live in the guard, so the transform injects a tracking touch and the
%% slot tracks `show` and stays reactive (not frozen).
cond_if_bare_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_if_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Show = arizona_template:get(show, Bindings), "
        "    arizona_template:html({'div', [], ["
        "        if "
        "            Show -> {'p', [], [<<\"yes\">>]}; "
        "            true -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    %% Bare element in the `if` tail compiles to a nested template; both branches render.
    {Shown, _} = arizona_render:render(Mod:render(#{show => true})),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Shown), <<"<p>yes</p>">>)),
    {Hidden, _} = arizona_render:render(Mod:render(#{show => false})),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Hidden), <<"<p">>)),
    %% the if-condition's binding is auto-tracked, so the slot is reactive.
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{show => true}), #{}),
    ?assertEqual([#{show => true}], maps:get(deps, Snap)).

%% A bare element in the tail of a nested `case`-of-`case` is reached by the
%% recursive tail walk.
cond_nested_case_bare_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(a, Bindings) of "
        "            1 -> "
        "                case arizona_template:get(b, Bindings) of "
        "                    2 -> {'p', [], [<<\"both\">>]}; "
        "                    _ -> <<\"\">> "
        "                end; "
        "            _ -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    {Hit, _} = arizona_render:render(Mod:render(#{a => 1, b => 2})),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Hit), <<"<p>both</p>">>)),
    {Miss, _} = arizona_render:render(Mod:render(#{a => 1, b => 9})),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Miss), <<"<p">>)).

%% Under ?terminal the branch element inherits the terminal backend (ANSI, not
%% HTML): proving the render target threads through the tail walk.
cond_case_terminal_inherits_target(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_term). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal({col, [], ["
        "        case arizona_template:get(warn, Bindings) of "
        "            true -> {line, [green], [arizona_template:get(msg, Bindings)]}; "
        "            false -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    {Output, _Snap} = arizona_render:render(Mod:render(#{warn => true, msg => ~"hi"})),
    Bin = iolist_to_binary(Output),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"\e[32m")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"hi")),
    ?assertEqual(nomatch, binary:match(Bin, ~"<line")).

%% Live diff: the conditional slot transitions empty (scalar) <-> element branch
%% across renders -- the real login-error appear/disappear behaviour. make_op
%% must handle both directions, scalar->template and template->scalar, and both
%% are the marker-aware OP_TEXT (the slot is anchored by comment markers).
cond_case_diff_transition(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_diff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(error, Bindings) of "
        "            undefined -> <<\"\">>; "
        "            Message -> {'p', [{class, <<\"login-error\">>}], [Message]} "
        "        end "
        "    ]}). "
    ),
    %% undefined -> message: the <p> appears (statics + dynamic in the payload)
    {_H0, Snap0} = arizona_render:render(Mod:render(#{error => undefined})),
    {AppearOps, Snap1} = arizona_diff:diff(Mod:render(#{error => <<"boom">>}), Snap0),
    AppearStr = iolist_to_binary(io_lib:format("~p", [AppearOps])),
    ?assertNotEqual([], AppearOps),
    ?assertNotEqual(nomatch, binary:match(AppearStr, <<"login-error">>)),
    ?assertNotEqual(nomatch, binary:match(AppearStr, <<"boom">>)),
    %% message -> undefined: a clearing op is emitted, the old content is gone
    {ClearOps, _Snap2} = arizona_diff:diff(Mod:render(#{error => undefined}), Snap1),
    ClearStr = iolist_to_binary(io_lib:format("~p", [ClearOps])),
    ?assertNotEqual([], ClearOps),
    ?assertEqual(nomatch, binary:match(ClearStr, <<"login-error">>)).

%% A pure element-list tail is compiled as a fragment (every item an element).
cond_case_element_list_tail(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_list). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], ["
        "        case arizona_template:get(show, Bindings) of "
        "            true -> [{'li', [], [<<\"a\">>]}, {'li', [], [<<\"b\">>]}]; "
        "            false -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    {HTML, _} = arizona_render:render(Mod:render(#{show => true})),
    Bin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"<li>a</li>">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"<li>b</li>">>)).

%% A mixed-fragment tail (static text interleaved with an element) is compiled
%% as a fragment -- matching what an explicit ?html([...]) there would do.
cond_case_mixed_fragment_tail(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(show, Bindings) of "
        "            true -> [<<\"label: \">>, {'b', [], [arizona_template:get(v, Bindings)]}]; "
        "            false -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    {HTML, _} = arizona_render:render(Mod:render(#{show => true, v => <<"x">>})),
    Bin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"label: ">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"<b ">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"x">>)).

%% A `begin ... end` block whose last expression is a bare element.
cond_begin_block_tail(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_begin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        begin "
        "            Msg = arizona_template:get(msg, Bindings), "
        "            {'p', [], [Msg]} "
        "        end "
        "    ]}). "
    ),
    {HTML, _} = arizona_render:render(Mod:render(#{msg => <<"hi">>})),
    Bin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"<p ">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"hi">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"</p>">>)).

%% A `try` body (no `of` clauses) is a tail -- a bare element there is expanded.
cond_try_bare_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_try). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        try {'p', [], [arizona_template:get(msg, Bindings)]} "
        "        catch _:_ -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    {HTML, _} = arizona_render:render(Mod:render(#{msg => <<"ok">>})),
    Bin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"<p ">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"ok">>)).

%% A `maybe ... else ... end`: the body's last expression and the else-clause
%% bodies are tails.
cond_maybe_bare_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_maybe). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        maybe "
        "            {ok, M} ?= arizona_template:get(result, Bindings), "
        "            {'p', [], [M]} "
        "        else _ -> <<\"\">> "
        "        end "
        "    ]}). "
    ),
    {HTML, _} = arizona_render:render(Mod:render(#{result => {ok, <<"hi">>}})),
    Bin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"<p ">>)),
    ?assertNotEqual(nomatch, binary:match(Bin, <<"hi">>)),
    {Empty, _} = arizona_render:render(Mod:render(#{result => error})),
    ?assertEqual(nomatch, binary:match(iolist_to_binary(Empty), <<"<p ">>)).

%% A `receive ... after ... end`: the `after` body is a tail (the receive
%% clauses are too); `after 0` fires immediately so SSR never blocks.
cond_receive_bare_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_cond_receive). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        receive "
        "            never -> <<\"\">> "
        "        after 0 -> {'p', [], [<<\"timeout\">>]} "
        "        end "
        "    ]}). "
    ),
    {HTML, _} = arizona_render:render(Mod:render(#{})),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), <<"<p>timeout</p>">>)).

%% Test 14: Two dynamic attributes -- both share az, statics split between both.
two_dynamic_attrs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_two_attrs). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, arizona_template:get(cls, Bindings, <<\"a\">>)}, "
        "                 {id, arizona_template:get(id, Bindings, <<\"b\">>)}], "
        "         [<<\"content\">>]}"
        "    ). "
    ),
    T = Mod:render(#{cls => <<"x">>, id => <<"y">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<>>,
                <<">content</div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [
        {Az0, {attr, <<"class">>, ClsFun}, {pt_two_attrs, 1}},
        {Az0, {attr, <<"id">>, IdFun}, {pt_two_attrs, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"x">>, ClsFun()),
    ?assertEqual(<<"y">>, IdFun()).

%% Test 15: Two dynamic text children -- compound markers, independently diffable.
two_dynamic_texts(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_two_texts). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [arizona_template:get(a, Bindings, <<\"x\">>), "
        "                   arizona_template:get(b, Bindings, <<\"y\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"hello">>, b => <<"world">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<p az=\"0\"><!--az:0-->">>,
                <<"<!--/az--><!--az:0:1-->">>,
                <<"<!--/az--></p>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    Az01 = <<Fp/binary, "-0:1">>,
    [{Az0, {esc, FunA}, {pt_two_texts, 1}}, {Az01, {esc, FunB}, {pt_two_texts, 1}}] = maps:get(
        d, T
    ),
    ?assertEqual(<<"hello">>, FunA()),
    ?assertEqual(<<"world">>, FunB()),
    %% Integration: diff produces independent ops for each dynamic
    {HTML, Snap} = arizona_render:render(T),
    ExpectedHTML = iolist_to_binary(
        scope_s(
            Fp,
            [<<"<p az=\"0\"><!--az:0-->hello<!--/az--><!--az:0:1-->world<!--/az--></p>">>]
        )
    ),
    ?assertEqual(ExpectedHTML, iolist_to_binary(HTML)),
    T2 = Mod:render(#{a => <<"A">>, b => <<"B">>}),
    {Ops, _} = arizona_diff:diff(T2, Snap),
    ?assertEqual(2, length(Ops)),
    ?assert(
        lists:any(
            fun
                ([0, Target, <<"A">>]) -> Target =:= Az0;
                (_) -> false
            end,
            Ops
        )
    ),
    ?assert(
        lists:any(
            fun
                ([0, Target, <<"B">>]) -> Target =:= Az01;
                (_) -> false
            end,
            Ops
        )
    ).

%% Test 16: Static text between two dynamics -- each with unique marker.
static_between_dynamics(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_between). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [arizona_template:get(a, Bindings, <<\"x\">>), "
        "                   <<\" and \">>, "
        "                   arizona_template:get(b, Bindings, <<\"y\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"hello">>, b => <<"world">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<p az=\"0\"><!--az:0-->">>,
                <<"<!--/az--> and <!--az:0:1-->">>,
                <<"<!--/az--></p>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    Az01 = <<Fp/binary, "-0:1">>,
    [{Az0, {esc, FunA}, {pt_between, 1}}, {Az01, {esc, FunB}, {pt_between, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, FunA()),
    ?assertEqual(<<"world">>, FunB()).

%% Test 17: Case expression as dynamic child.
case_expression_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_case). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], [case arizona_template:get(x, Bindings, true) of "
        "            true -> <<\"yes\">>; "
        "            false -> <<\"no\">> "
        "        end]}"
        "    ). "
    ),
    T = Mod:render(#{x => true}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<span az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></span>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_case, 1}}] = maps:get(d, T),
    ?assertEqual(<<"yes">>, Fun()).

%% Test 18: Variable reference as dynamic child.
variable_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_var). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Val = arizona_template:get(val, Bindings, <<\"default\">>), "
        "    arizona_template:html("
        "        {'p', [], [Val]}"
        "    ). "
    ),
    T = Mod:render(#{val => <<"hello">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<p az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></p>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_var, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 19: arizona:stateful/2 inside template stays as dynamic (not compile-time transformed).
stateful_child_not_transformed(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_stateful_child). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], [arizona_template:stateful(child_mod, #{count => 0})]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, Fun, {pt_stateful_child, 1}}] = maps:get(d, T),
    Result = Fun(),
    ?assertEqual(#{stateful => child_mod, props => #{count => 0}}, Result).

%% Test 20: Multiple arizona:template/1 calls in case branches.
multiple_stateful_calls(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_multi_stateful). "
        "-export([render/2]). "
        "render(Bindings, Type) -> "
        "    case Type of "
        "        a -> arizona_template:html("
        "            {'p', [], [arizona_template:get(x, Bindings, <<\"a\">>)]}); "
        "        b -> arizona_template:html("
        "            {'span', [], [arizona_template:get(x, Bindings, <<\"b\">>)]}) "
        "    end. "
    ),
    TA = Mod:render(#{x => <<"hello">>}, a),
    FpA = maps:get(f, TA),
    ?assertEqual(
        scope_s(
            FpA,
            [<<"<p az=\"0\"><!--az:0-->">>, <<"<!--/az--></p>">>]
        ),
        maps:get(s, TA)
    ),
    TB = Mod:render(#{x => <<"world">>}, b),
    FpB = maps:get(f, TB),
    ?assertEqual(
        scope_s(
            FpB,
            [<<"<span az=\"0\"><!--az:0-->">>, <<"<!--/az--></span>">>]
        ),
        maps:get(s, TB)
    ).

%% Test 21: 3 levels nesting -- az counter threading at depth.
deeply_nested(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_deep). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            {'section', [], ["
        "                {'p', [], [arizona_template:get(a, Bindings, <<\"x\">>)]}"
        "            ]},"
        "            {'p', [], [arizona_template:get(b, Bindings, <<\"y\">>)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"deep">>, b => <<"shallow">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<
                    "<div>"
                    "<section>"
                    "<p az=\"0\"><!--az:0-->"
                >>,
                <<
                    "<!--/az--></p>"
                    "</section>"
                    "<p az=\"1\"><!--az:1-->"
                >>,
                <<
                    "<!--/az--></p>"
                    "</div>"
                >>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [{Az0, {esc, FunA}, {pt_deep, 1}}, {Az1, {esc, FunB}, {pt_deep, 1}}] = maps:get(d, T),
    ?assertEqual(<<"deep">>, FunA()),
    ?assertEqual(<<"shallow">>, FunB()).

%% Test 22: Single dynamic child, no static prefix.
dynamic_only_child(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_dyn_only). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], [arizona_template:get(x, Bindings, <<\"val\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{x => <<"test">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<span az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></span>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_dyn_only, 1}}] = maps:get(d, T),
    ?assertEqual(<<"test">>, Fun()).

%% Test 23: Same bindings produce no diff ops.
no_change_diff(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [arizona_template:get(x, Bindings, <<\"a\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{x => <<"hello">>}),
    {_HTML, Snap} = arizona_render:render(T),
    T2 = Mod:render(#{x => <<"hello">>}),
    {Ops, _} = arizona_diff:diff(T2, Snap),
    ?assertEqual([], Ops).

%% Test 24: Bare void element -- no attrs, no children.
void_no_attrs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_void_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'br', [], []}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<br />">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% HTML tag names are ASCII case-insensitive, so `{'BR', ...}` is the void element
%% `br` to the browser. Classifying only the lowercase atom emitted `<BR></BR>` --
%% malformed markup, since a void element has no end tag. The tag's own casing is
%% preserved in the output (name/1 never rewrites it, which is what keeps a
%% case-sensitive SVG attribute like viewBox intact); only the classification is
%% case-insensitive.
void_tag_case_insensitive(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_void_case). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'BR', [], []}, "
        "        {'Img', [{src, <<\"x\">>}], []}, "
        "        {'br', [], []}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(<<"<div><BR /><Img src=\"x\" /><br /></div>">>, HTML),
    %% No end tag was emitted for any of them.
    ?assertEqual(nomatch, binary:match(HTML, ~"</BR>")),
    ?assertEqual(nomatch, binary:match(HTML, ~"</Img>")),
    ?assertEqual(nomatch, binary:match(HTML, ~"</br>")),
    %% The classifier itself, over the shapes the sweep covers.
    ?assert(arizona_html:is_void('BR')),
    ?assert(arizona_html:is_void('Img')),
    ?assert(arizona_html:is_void(input)),
    %% Control: a non-void tag stays non-void in every casing.
    ?assertNot(arizona_html:is_void('DIV')),
    ?assertNot(arizona_html:is_void('div')).

%% ...and the void-with-children rejection follows the classification, so an
%% uppercase void tag can no longer smuggle children past it.
void_tag_case_insensitive_rejects_children(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_void_case_children). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'BR', [], [<<\"x\">>]}). ",
        fun
            ({void_with_children, 'BR'}) -> true;
            (_) -> false
        end
    ).

%% The reserved-name check exists because a template-authored `az` can collide with
%% a genuine slot address and misroute a patch. HTML attribute names are ASCII
%% case-insensitive, so `AZ="x"` IS an `az` attribute to the browser (the client
%% resolves slots with querySelector('[az=...]'), which matches names
%% case-insensitively) -- matching only the lowercase form let the exact harm the
%% check prevents walk straight through it.
reserved_attr_case_insensitive(Config) when is_list(Config) ->
    Reserved = [
        {"pt_res_AZ", "{'AZ', <<\"x\">>}"},
        {"pt_res_azl", "{'Az-Local', <<\"x\">>}"},
        {"pt_res_AZ_UND", "{'AZ_LOCAL', <<\"x\">>}"},
        {"pt_res_bin", "{<<\"AZ\">>, <<\"x\">>}"},
        %% Lowercase controls -- unchanged behaviour.
        {"pt_res_az", "{az, <<\"x\">>}"},
        {"pt_res_azl_low", "{'az-local', <<\"x\">>}"}
    ],
    lists:foreach(
        fun({Mod, Attr}) ->
            assert_parse_error(
                lists:flatten([
                    "-module(",
                    Mod,
                    "). -export([render/1]). render(Bindings) -> ",
                    "arizona_template:html({'p', [",
                    Attr,
                    "], []}). "
                ]),
                fun
                    ({reserved_attr, _}) -> true;
                    (_) -> false
                end
            )
        end,
        Reserved
    ),
    %% Control: every OTHER az-* name is the author's, in any casing.
    Mod = compile_module(
        "-module(pt_res_ok). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [{'AZ-KEY', <<\"k\">>}, {az_click, <<\"c\">>}], []}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{})),
    ?assertEqual(<<"<p AZ-KEY=\"k\" az-click=\"c\"></p>">>, iolist_to_binary(HTML0)).

%% az-view is injected on a live root and rejected anywhere else, for the same
%% reason: the browser reads `AZ-VIEW="x"` as `az-view`.
az_view_attr_case_insensitive(Config) when is_list(Config) ->
    lists:foreach(
        fun({Mod, Attr}) ->
            assert_parse_error(
                lists:flatten([
                    "-module(",
                    Mod,
                    "). -export([render/1]). render(Bindings) -> ",
                    "arizona_template:html({'p', [",
                    Attr,
                    "], []}). "
                ]),
                fun(R) -> R =:= az_view_not_allowed end
            )
        end,
        [
            {"pt_azv_up", "{'AZ-VIEW', <<\"x\">>}"},
            {"pt_azv_und", "{'AZ_VIEW', <<\"x\">>}"},
            {"pt_azv_bare", "'Az-View'"},
            {"pt_azv_bin", "<<\"AZ-VIEW\">>"},
            %% Lowercase controls -- unchanged behaviour.
            {"pt_azv_low", "{'az-view', <<\"x\">>}"},
            {"pt_azv_low_bare", "az_view"}
        ]
    ).

%% az-nodiff is the one where matching the lowercase form only produced the
%% OPPOSITE of the request: unrecognized, the directive fell through as an ordinary
%% boolean attribute, so the element kept its az and markers (still diffed) AND
%% leaked `AZ-NODIFF` into the DOM. Assert the directive is HONORED, not just
%% stripped.
nodiff_directive_case_insensitive(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_case). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['AZ-NODIFF'], [arizona_template:get(count, Bindings, 0)]}"
        "    ). "
    ),
    T = Mod:render(#{count => 42}),
    %% Honored: the compile unit is marked non-diffable...
    ?assertEqual(false, maps:get(diff, T)),
    %% ...its dynamic carries no diff address...
    ?assertMatch(
        [{undefined, {attr, ~"az-nodiff", _}, _}, {undefined, _Spec, _Loc}], maps:get(d, T)
    ),
    {HTML0, _Snap} = arizona_render:render(T),
    HTML = iolist_to_binary(HTML0),
    %% ...and the rendered element has no az attribute and no slot markers; the
    %% directive stays, lowercase, holding the az-nodiff element token the compiler owns.
    #{f := F} = T,
    ?assertEqual(<<"<div az-nodiff=\"", F/binary, "\">42</div>">>, HTML),
    ?assertEqual(nomatch, binary:match(HTML, ~"AZ-NODIFF")),
    ?assertEqual(nomatch, binary:match(HTML, ~" az=")),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    %% The underscore spelling folds case too, and a nested one is still rejected
    %% (the directive is whole-template, so recognizing it must reject it there).
    assert_parse_error(
        "-module(pt_nodiff_case_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {'p', ['AZ_NODIFF'], [arizona_template:get(v, Bindings)]}"
        "    ]}). ",
        fun(R) -> R =:= nested_nodiff end
    ).

%% Test 25: Static attrs after dynamic attr.
static_after_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_static_after). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'input', [{value, arizona_template:get(v, Bindings, <<\"\">>)}, "
        "                   {type, <<\"text\">>}], []}"
        "    ). "
    ),
    T = Mod:render(#{v => <<"hello">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<input az=\"0\"">>,
                <<" type=\"text\" />">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {attr, <<"value">>, Fun}, {pt_static_after, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 26: template/2 basic -- single dynamic child per item.
template2_basic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_basic). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'li', [], [maps:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [#{name => <<"Alice">>}]}),
    %% template/2 returns arizona_template:each(Source, Tmpl),
    %% which is #{t => 0, source => Source, template => Tmpl}
    ?assertEqual(0, maps:get(t, Result)),
    ?assert(is_map(Result)),
    Tmpl = maps:get(template, Result),
    ?assertEqual(0, maps:get(t, Tmpl)),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<li az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></li>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    ?assert(is_function(DFun, 1)),
    Dynamics = DFun(#{name => <<"Alice">>}),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_each_basic, 1}}] = Dynamics,
    ?assertEqual(<<"Alice">>, Fun()).

%% Test 27: template/2 with az-key and other dynamic attrs.
template2_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'div', [{<<\"az-key\">>, maps:get(id, Item)}, "
        "                 {class, maps:get(cls, Item)}], "
        "         [maps:get(text, Item)]} "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<>>,
                <<"><!--az:0-->">>,
                <<"<!--/az--></div>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Dynamics = DFun(#{id => <<"r1">>, cls => <<"active">>, text => <<"hello">>}),
    Az0 = <<Fp/binary, "-0">>,
    [
        {Az0, {attr, <<"az-key">>, KeyFun}, {pt_each_attr, 1}},
        {Az0, {attr, <<"class">>, ClsFun}, {pt_each_attr, 1}},
        {Az0, {esc, TextFun}, {pt_each_attr, 1}}
    ] = Dynamics,
    ?assertEqual(<<"r1">>, KeyFun()),
    ?assertEqual(<<"active">>, ClsFun()),
    ?assertEqual(<<"hello">>, TextFun()).

%% Test 28: template/2 with multiple td children, each with dynamics.
template2_multiple_children(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Row) -> "
        "        {'tr', [{<<\"az-key\">>, maps:get(id, Row)}], ["
        "            {'td', [], [maps:get(name, Row)]},"
        "            {'td', [], [maps:get(age, Row)]}"
        "        ]} "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<tr az=\"0\"">>,
                <<
                    ">"
                    "<td az=\"1\"><!--az:1-->"
                >>,
                <<
                    "<!--/az--></td>"
                    "<td az=\"2\"><!--az:2-->"
                >>,
                <<
                    "<!--/az--></td>"
                    "</tr>"
                >>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Dynamics = DFun(#{id => <<"1">>, name => <<"Bob">>, age => 30}),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    Az2 = <<Fp/binary, "-2">>,
    [
        {Az0, {attr, <<"az-key">>, KeyFun}, {pt_each_multi, 1}},
        {Az1, {esc, NameFun}, {pt_each_multi, 1}},
        {Az2, {esc, AgeFun}, {pt_each_multi, 1}}
    ] = Dynamics,
    ?assertEqual(<<"1">>, KeyFun()),
    ?assertEqual(<<"Bob">>, NameFun()),
    ?assertEqual(30, AgeFun()).

%% Test 29: template/2 nested inside template/1 -- common real-world case.
template2_nested_in_template(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, arizona_template:get(id, Bindings, <<\"list\">>)}], ["
        "            arizona_template:each(fun(Item) -> "
        "                {'li', [], [maps:get(text, Item)]} "
        "            end, arizona_template:get(items, Bindings, []))"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{id => <<"mylist">>, items => [#{text => <<"hi">>}]}),
    Fp = maps:get(f, T),
    %% Outer template is #{s, d, f}
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<"><!--az:0-->">>,
                <<"<!--/az--></div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [
        {Az0, {attr, <<"id">>, IdFun}, {pt_each_nested, 1}},
        {Az0, EachFun, {pt_each_nested, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"mylist">>, IdFun()),
    %% The each fun returns an each descriptor
    EachResult = EachFun(),
    ?assertEqual(0, maps:get(t, EachResult)),
    Tmpl = maps:get(template, EachResult),
    DFun = maps:get(d, Tmpl),
    InnerFp = maps:get(f, Tmpl),
    InnerAz0 = <<InnerFp/binary, "-0">>,
    [{InnerAz0, {esc, ItemFun}, {pt_each_nested, 1}}] = DFun(#{text => <<"hi">>}),
    ?assertEqual(<<"hi">>, ItemFun()).

%% Test 30: template/2 with mix of static text and dynamic children.
template2_static_and_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'p', [], [<<\"Name: \">>, maps:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<p az=\"0\">Name: <!--az:0-->">>,
                <<"<!--/az--></p>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_each_mixed, 1}}] = DFun(#{name => <<"Eve">>}),
    ?assertEqual(<<"Eve">>, Fun()).

%% Test 31: template/2 render integration -- arizona:render/1 produces correct HTML.
template2_render_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [], ["
        "            arizona_template:each(fun(Item) -> "
        "                {'li', [{<<\"az-key\">>, maps:get(id, Item)}], ["
        "                    maps:get(name, Item)"
        "                ]} "
        "            end, arizona_template:get(items, Bindings, []))"
        "        ]}"
        "    ). "
    ),
    Items = [
        #{id => <<"1">>, name => <<"Alice">>},
        #{id => <<"2">>, name => <<"Bob">>}
    ],
    Tmpl = Mod:render(#{items => Items}),
    OuterFp = maps:get(f, Tmpl),
    OuterAz0 = <<OuterFp/binary, "-0">>,
    %% Get inner template fp -- only one dynamic (the each)
    [{OuterAz0, EachFun, {pt_each_render, 1}}] = maps:get(d, Tmpl),
    EachDesc = EachFun(),
    InnerTmpl = maps:get(template, EachDesc),
    InnerFp = maps:get(f, InnerTmpl),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    Expected = iolist_to_binary([
        <<"<ul az=\"", OuterFp/binary, "-0\"><!--az:", OuterFp/binary, "-0-->">>,
        <<"<li az=\"", InnerFp/binary, "-0\" az-key=\"1\"><!--az:", InnerFp/binary,
            "-0-->Alice<!--/az--></li>">>,
        <<"<li az=\"", InnerFp/binary, "-0\" az-key=\"2\"><!--az:", InnerFp/binary,
            "-0-->Bob<!--/az--></li>">>,
        <<"<!--/az--></ul>">>
    ]),
    ?assertEqual(Expected, iolist_to_binary(HTML)).

%% Test 32: template/2 two dynamic text children use compound markers.
template2_compound_markers(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_compound). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'span', [], [maps:get(first, Item), <<\" \">>, maps:get(last, Item)]} "
        "    end, arizona_template:get(people, Bindings, [])). "
    ),
    Result = Mod:render(#{people => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<span az=\"0\"><!--az:0-->">>,
                <<"<!--/az--> <!--az:0:1-->">>,
                <<"<!--/az--></span>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Dynamics = DFun(#{first => <<"John">>, last => <<"Doe">>}),
    Az0 = <<Fp/binary, "-0">>,
    Az01 = <<Fp/binary, "-0:1">>,
    [{Az0, {esc, FirstFun}, {pt_each_compound, 1}}, {Az01, {esc, LastFun}, {pt_each_compound, 1}}] =
        Dynamics,
    ?assertEqual(<<"John">>, FirstFun()),
    ?assertEqual(<<"Doe">>, LastFun()).

%% Test 33: template/2 with multi-expression fun body.
template2_multi_expr_body(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_multi_expr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        Name = maps:get(name, Item), "
        "        Upper = string:uppercase(Name), "
        "        {'li', [], [Upper]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<li az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></li>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_each_multi_expr, 1}}] = DFun(#{name => <<"alice">>}),
    ?assertEqual(<<"ALICE">>, Fun()).

%% Test 34: template/2 with multi-expression body and dynamic attr.
template2_multi_expr_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_multi_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        Id = maps:get(id, Item), "
        "        {'div', [{class, Id}], [maps:get(text, Item)]} "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    DFun = maps:get(d, Tmpl),
    Dynamics = DFun(#{id => <<"x">>, text => <<"hi">>}),
    Az0 = <<Fp/binary, "-0">>,
    [
        {Az0, {attr, <<"class">>, ClsFun}, {pt_each_multi_attr, 1}},
        {Az0, {esc, TextFun}, {pt_each_multi_attr, 1}}
    ] = Dynamics,
    ?assertEqual(<<"x">>, ClsFun()),
    ?assertEqual(<<"hi">>, TextFun()).

%% Test 35: template/2 with guard in fun clause.
template2_guard(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_guard). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) when is_map(Item) -> "
        "        {'li', [], [maps:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_each_guard, 1}}] = DFun(#{name => <<"Bob">>}),
    ?assertEqual(<<"Bob">>, Fun()).

%% Test 36: template/2 with static-only item -- no dynamics in item.
template2_static_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_static_item). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(_Item) -> "
        "        {'li', [{class, <<\"row\">>}], [<<\"fixed\">>]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [a, b]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual([<<"<li class=\"row\">fixed</li>">>], maps:get(s, Tmpl)),
    DFun = maps:get(d, Tmpl),
    ?assertEqual([], DFun(anything)).

%% Test 37: template/2 with void element in item.
template2_void_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_void). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'input', [{type, <<\"text\">>}, {value, maps:get(val, Item)}], []} "
        "    end, arizona_template:get(fields, Bindings, [])). "
    ),
    Result = Mod:render(#{fields => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<input az=\"0\" type=\"text\"">>,
                <<" />">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {attr, <<"value">>, Fun}, {pt_each_void, 1}}] = DFun(#{val => <<"hi">>}),
    ?assertEqual(<<"hi">>, Fun()).

%% Test 38: template/1 nested inside template/2's fun body.
template2_inner_template1(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_inner_t1). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'div', [], ["
        "            arizona_template:html({'span', [], [maps:get(x, Item)]})"
        "        ]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    %% Outer item element has a dynamic child (the inner template map)
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></div>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, Fun, {pt_each_inner_t1, 1}}] = DFun(#{x => <<"hello">>}),
    %% The dynamic returns a nested template map
    InnerTmpl = Fun(),
    InnerFp = maps:get(f, InnerTmpl),
    ?assertEqual(
        scope_s(
            InnerFp,
            [
                <<"<span az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></span>">>
            ]
        ),
        maps:get(s, InnerTmpl)
    ),
    InnerAz0 = <<InnerFp/binary, "-0">>,
    [{InnerAz0, {esc, InnerFun}, {pt_each_inner_t1, 1}}] = maps:get(d, InnerTmpl),
    ?assertEqual(<<"hello">>, InnerFun()).

%% Test 39: template/2 render integration with empty source list.
template2_empty_list_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_empty). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [], ["
        "            arizona_template:each(fun(Item) -> "
        "                {'li', [], [maps:get(name, Item)]} "
        "            end, arizona_template:get(items, Bindings, []))"
        "        ]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{items => []}),
    Fp = maps:get(f, Tmpl),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    Expected = iolist_to_binary(
        scope_s(
            Fp,
            [<<"<ul az=\"0\"><!--az:0--><!--/az--></ul>">>]
        )
    ),
    ?assertEqual(Expected, iolist_to_binary(HTML)).

%% Test 40: template/2 diff integration -- changed items produce correct ops.
template2_diff_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_diff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            arizona_template:get(title, Bindings, <<\"default\">>),"
        "            arizona_template:each(fun(Item) -> "
        "                {'p', [], [maps:get(text, Item)]} "
        "            end, arizona_template:get(items, Bindings, []))"
        "        ]}"
        "    ). "
    ),
    %% First render
    T1 = Mod:render(#{title => <<"hello">>, items => [#{text => <<"a">>}]}),
    {_HTML, Snap} = arizona_render:render(T1),
    %% Update title only -- should get a text op
    T2 = Mod:render(#{title => <<"world">>, items => [#{text => <<"a">>}]}),
    {Ops, _NewSnap} = arizona_diff:diff(T2, Snap),
    ?assert(
        lists:any(
            fun
                ([0, _, <<"world">>]) -> true;
                (_) -> false
            end,
            Ops
        )
    ).

%% Test 41: template/1 with fragment (list of element tuples).
template1_fragment(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fragment). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html([{td, [], [arizona_template:get(a, Bindings, <<\"x\">>)]}, "
        "                      {td, [], [arizona_template:get(b, Bindings, <<\"y\">>)]}]). "
    ),
    T = Mod:render(#{a => <<"A">>, b => <<"B">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<td az=\"0\"><!--az:0-->">>,
                <<
                    "<!--/az--></td>"
                    "<td az=\"1\"><!--az:1-->"
                >>,
                <<"<!--/az--></td>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [{Az0, {esc, FunA}, {pt_fragment, 1}}, {Az1, {esc, FunB}, {pt_fragment, 1}}] = maps:get(d, T),
    ?assertEqual(<<"A">>, FunA()),
    ?assertEqual(<<"B">>, FunB()).

%% Test 42: template/1 fragment renders to correct HTML via arizona:render/1.
template1_fragment_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fragment_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html([{td, [], [arizona_template:get(a, Bindings, <<\"x\">>)]}, "
        "                      {td, [], [<<\"static\">>]}]). "
    ),
    Tmpl = Mod:render(#{a => <<"hello">>}),
    Fp = maps:get(f, Tmpl),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    Expected = iolist_to_binary(
        scope_s(
            Fp,
            [<<"<td az=\"0\"><!--az:0-->hello<!--/az--></td><td>static</td>">>]
        )
    ),
    ?assertEqual(Expected, iolist_to_binary(HTML)).

%% Test 43: a bare-value (non-element) ?each body is rejected -- ?each needs a per-item
%% element; for plain values use a comprehension.
template2_non_element_expr(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_non_elem). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        maps:get(name, Item) "
        "    end, arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% Test 44: the rejection fires even when the ?each is nested inside an ?html element.
template2_non_element_render(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_non_elem_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [], ["
        "            arizona_template:each(fun(Item) -> "
        "                maps:get(name, Item) "
        "            end, arizona_template:get(items, Bindings, []))"
        "        ]}"
        "    ). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% Test 45: template/2 with static binary body -- no dynamics.
template2_static_binary_body(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_static_bin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(_Item) -> "
        "        <<\"hello\">> "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => []}),
    Tmpl = maps:get(template, Result),
    ?assertEqual([<<"hello">>], maps:get(s, Tmpl)),
    DFun = maps:get(d, Tmpl),
    ?assertEqual([], DFun(anything)).

%% Test 46: a prefix does not exempt -- a non-element last expr is still rejected.
template2_non_element_multi_expr(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_non_elem_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        Name = maps:get(name, Item), "
        "        string:uppercase(Name) "
        "    end, arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% Test 47: template/1 with static binary -- no element, no dynamics.
template1_static_binary(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_static_bin). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html(<<\"hello\">>). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"hello">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)),
    ?assert(is_binary(maps:get(f, T))).

%% Test 48: template/1 with dynamic expression -- fallback path. The root slot
%% is marker-anchored in the statics (previously `[<<>>, <<>>]`, which left its
%% diff ops targeting an az that existed nowhere in the DOM).
template1_dynamic_expr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_dyn_expr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(arizona_template:get(content, Bindings, <<\"default\">>)). "
    ),
    T = Mod:render(#{content => <<"hello">>}),
    Fp = maps:get(f, T),
    Az0 = <<Fp/binary, "-0">>,
    ?assertEqual([<<"<!--az:", Az0/binary, "-->">>, <<"<!--/az-->">>], maps:get(s, T)),
    [{Az0, {esc, Fun}, {pt_dyn_expr, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 49: template/1 with dynamic expression -- render integration. The
%% rendered value sits inside the root slot's markers, and the diff op targets
%% that anchored marker.
template1_dynamic_expr_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_dyn_expr_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(arizona_template:get(content, Bindings, <<\"default\">>)). "
    ),
    Tmpl = Mod:render(#{content => <<"hello">>}),
    Fp = maps:get(f, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    {HTML, Snap} = arizona_render:render(Tmpl),
    HTMLBin = iolist_to_binary(HTML),
    ?assertEqual(<<"<!--az:", Az0/binary, "-->hello<!--/az-->">>, HTMLBin),
    %% Diff works, and its target is the marker SSR just rendered.
    Tmpl2 = Mod:render(#{content => <<"world">>}),
    {Ops, _} = arizona_diff:diff(Tmpl2, Snap),
    ?assertEqual([[0, Az0, <<"world">>]], Ops),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<!--az:", Az0/binary, "-->">>)).

%% Test 50: template/1 with static-only fragment -- no dynamics in any element.
template1_static_fragment(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_static_frag). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html([{td, [], [<<\"a\">>]}, {td, [], [<<\"b\">>]}]). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<td>a</td><td>b</td>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% Test 51: template/2 with fragment body -- list of element tuples in fun.
template2_fragment_body(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_frag). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        [{td, [], [maps:get(name, Item)]}, "
        "         {td, [], [maps:get(age, Item)]}] "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<td az=\"0\"><!--az:0-->">>,
                <<
                    "<!--/az--></td>"
                    "<td az=\"1\"><!--az:1-->"
                >>,
                <<"<!--/az--></td>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [{Az0, {esc, NameFun}, {pt_each_frag, 1}}, {Az1, {esc, AgeFun}, {pt_each_frag, 1}}] = DFun(#{
        name => <<"Alice">>, age => 30
    }),
    ?assertEqual(<<"Alice">>, NameFun()),
    ?assertEqual(30, AgeFun()).

%% Test 52: template/2 fragment body -- render integration.
template2_fragment_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_frag_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'table', [], ["
        "            arizona_template:each(fun(Item) -> "
        "                [{td, [], [maps:get(name, Item)]}, "
        "                 {td, [], [maps:get(age, Item)]}] "
        "            end, arizona_template:get(rows, Bindings, []))"
        "        ]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{rows => [#{name => <<"Alice">>, age => 30}]}),
    OuterFp = maps:get(f, Tmpl),
    OuterAz0 = <<OuterFp/binary, "-0">>,
    %% Get inner template fp -- only one dynamic (the each)
    [{OuterAz0, EachFun, {pt_each_frag_render, 1}}] = maps:get(d, Tmpl),
    EachDesc = EachFun(),
    InnerTmpl = maps:get(template, EachDesc),
    InnerFp = maps:get(f, InnerTmpl),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    Expected = iolist_to_binary([
        <<"<table az=\"", OuterFp/binary, "-0\"><!--az:", OuterFp/binary, "-0-->">>,
        <<"<td az=\"", InnerFp/binary, "-0\"><!--az:", InnerFp/binary,
            "-0-->Alice<!--/az--></td>">>,
        <<"<td az=\"", InnerFp/binary, "-1\"><!--az:", InnerFp/binary, "-1-->30<!--/az--></td>">>,
        <<"<!--/az--></table>">>
    ]),
    ?assertEqual(Expected, iolist_to_binary(HTML)).

%% Test 53: template/2 with fragment body and multi-expression prefix.
template2_fragment_multi_expr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_frag_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        Name = maps:get(name, Item), "
        "        Upper = string:uppercase(Name), "
        "        [{td, [], [Upper]}, "
        "         {td, [], [maps:get(age, Item)]}] "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [
        {Az0, {esc, NameFun}, {pt_each_frag_multi, 1}},
        {Az1, {esc, AgeFun}, {pt_each_frag_multi, 1}}
    ] = DFun(#{name => <<"alice">>, age => 25}),
    ?assertEqual(<<"ALICE">>, NameFun()),
    ?assertEqual(25, AgeFun()).

%% Test 54: template/2 with map pattern in fun argument.
template2_map_pattern(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_map_pat). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(#{name := Name, age := Age}) -> "
        "        {'tr', [], ["
        "            {'td', [], [Name]}, "
        "            {'td', [], [Age]}"
        "        ]} "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<
                    "<tr>"
                    "<td az=\"0\"><!--az:0-->"
                >>,
                <<
                    "<!--/az--></td>"
                    "<td az=\"1\"><!--az:1-->"
                >>,
                <<
                    "<!--/az--></td>"
                    "</tr>"
                >>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    [
        {Az0, {esc, NameFun}, {pt_each_map_pat, 1}},
        {Az1, {esc, AgeFun}, {pt_each_map_pat, 1}}
    ] = DFun(#{name => <<"Alice">>, age => 30}),
    ?assertEqual(<<"Alice">>, NameFun()),
    ?assertEqual(30, AgeFun()).

%% Test 55: a map-pattern head does not exempt -- a non-element body is still rejected.
template2_map_pattern_non_element(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_map_pat_ne). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(#{name := Name}) -> "
        "        Name "
        "    end, arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% Test 56: template/2 prefix with static body -- prefix runs, no dynamics.
template2_prefix_static_body(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_prefix_static). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        _Name = maps:get(name, Item), "
        "        {li, [{class, <<\"row\">>}], [<<\"fixed\">>]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [a]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual([<<"<li class=\"row\">fixed</li>">>], maps:get(s, Tmpl)),
    DFun = maps:get(d, Tmpl),
    ?assertEqual([], DFun(#{name => <<"x">>})).

%% Test 57: template/2 prefix variable used by some dynamics but not all.
template2_prefix_partial_use(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_prefix_partial). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(#{id := Id, name := Name, age := Age}) -> "
        "        IdBin = integer_to_binary(Id), "
        "        {'tr', [{<<\"az-key\">>, IdBin}], ["
        "            {'td', [], [IdBin]}, "
        "            {'td', [], [Name]}, "
        "            {'td', [], [Age]}"
        "        ]} "
        "    end, arizona_template:get(rows, Bindings, [])). "
    ),
    Result = Mod:render(#{rows => []}),
    Tmpl = maps:get(template, Result),
    Fp = maps:get(f, Tmpl),
    DFun = maps:get(d, Tmpl),
    Dynamics = DFun(#{id => 1, name => <<"Alice">>, age => 30}),
    Az0 = <<Fp/binary, "-0">>,
    Az1 = <<Fp/binary, "-1">>,
    Az2 = <<Fp/binary, "-2">>,
    Az3 = <<Fp/binary, "-3">>,
    [
        {Az0, {attr, <<"az-key">>, KeyFun}, {pt_each_prefix_partial, 1}},
        {Az1, {esc, IdFun}, {pt_each_prefix_partial, 1}},
        {Az2, {esc, NameFun}, {pt_each_prefix_partial, 1}},
        {Az3, {esc, AgeFun}, {pt_each_prefix_partial, 1}}
    ] = Dynamics,
    ?assertEqual(<<"1">>, KeyFun()),
    ?assertEqual(<<"1">>, IdFun()),
    ?assertEqual(<<"Alice">>, NameFun()),
    ?assertEqual(30, AgeFun()).

%% Test 58: az-nodiff atom directive → diff => false, not in HTML.
nodiff_atom(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_atom). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], [arizona_template:get(count, Bindings, 0)]}"
        "    ). "
    ),
    T = Mod:render(#{count => 42}),
    ?assertEqual(false, maps:get(diff, T)),
    %% az-nodiff must not appear in statics
    Statics = maps:get(s, T),
    StaticsBin = iolist_to_binary(Statics),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az-nodiff">>)).

%% Test 59: az-nodiff binary directive → same behavior.
nodiff_binary(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_bin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [<<\"az-nodiff\">>], [arizona_template:get(count, Bindings, 0)]}"
        "    ). "
    ),
    T = Mod:render(#{count => 42}),
    ?assertEqual(false, maps:get(diff, T)),
    Statics = maps:get(s, T),
    StaticsBin = iolist_to_binary(Statics),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az-nodiff">>)).

%% Test 60: No directive → no diff key.
nodiff_absent(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_absent). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], [arizona_template:get(count, Bindings, 0)]}"
        "    ). "
    ),
    T = Mod:render(#{count => 42}),
    ?assertNot(maps:is_key(diff, T)).

%% Test 61: diff => false integration -- diff/2 short-circuits.
nodiff_diff_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_int). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], [arizona_template:get(count, Bindings, 0)]}"
        "    ). "
    ),
    T1 = Mod:render(#{count => 1}),
    {_HTML, Snap} = arizona_render:render(T1),
    T2 = Mod:render(#{count => 2}),
    {Ops, _Snap2} = arizona_diff:diff(T2, Snap),
    ?assertEqual([], Ops).

%% Test 62: {hidden, true} → boolean attribute present.
bool_attr_true(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bool_true). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{hidden, true}], [<<\"content\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    ?assertEqual(<<"<div hidden>content</div>">>, Static).

%% Test 63: {hidden, false} → attribute stripped.
bool_attr_false(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bool_false). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{hidden, false}], [<<\"content\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    ?assertEqual(<<"<div>content</div>">>, Static).

%% Test 64: bare binary boolean -- ~"hidden" → <div hidden>.
bare_binary_bool(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bare_bin). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'div', [<<\"hidden\">>], [<<\"content\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    ?assertEqual(<<"<div hidden>content</div>">>, Static).

%% Dynamic bool: {checked, Expr} where Expr evaluates to true at runtime.
bool_dynamic_true(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bool_dyn_true). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'input', [{type, <<\"checkbox\">>}, {checked, maps:get(c, Bindings)}], []}"
        "    ). "
    ),
    T = Mod:render(#{c => true}),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertMatch(<<"<input", _/binary>>, HTML),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"checked">>)),
    ?assertEqual(nomatch, binary:match(HTML, <<"checked=\"">>)).

%% Dynamic bool: {checked, Expr} where Expr evaluates to false at runtime.
bool_dynamic_false(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bool_dyn_false). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'input', [{type, <<\"checkbox\">>}, {checked, maps:get(c, Bindings)}], []}"
        "    ). "
    ),
    T = Mod:render(#{c => false}),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertEqual(nomatch, binary:match(HTML, <<"checked">>)).

%% Dynamic bool: {checked, Expr} where Expr evaluates to a string value.
bool_dynamic_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bool_dyn_val). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, maps:get(cls, Bindings)}], [<<\"ok\">>]}"
        "    ). "
    ),
    T = Mod:render(#{cls => <<"active">>}),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"class=\"active\"">>)).

%% Test 65: az-nodiff coexists with other attrs -- directive stripped, others kept.
nodiff_with_other_attrs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, <<\"box\">>}, 'az-nodiff'], "
        "            [arizona_template:get(count, Bindings, 0)]}"
        "    ). "
    ),
    T = Mod:render(#{count => 5}),
    ?assertEqual(false, maps:get(diff, T)),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"class=\"box\"">>)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az-nodiff">>)).

%% Test 66: az-nodiff on template/2 (each) -- opts thread through build_each_ast.
nodiff_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'li', ['az-nodiff'], [maps:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [#{name => <<"a">>}]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual(false, maps:get(diff, Tmpl)),
    %% An unkeyed item keeps the directive bare: no token, no diff address.
    ?assertEqual([<<"<li az-nodiff>">>, <<"</li>">>], maps:get(s, Tmpl)).

%% az-nodiff reaches every template nested in the directive's compile unit: they
%% render inside its output, which never diffs, so none of them may carry a
%% framework marker either.
nodiff_inherited_branch(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_branch). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'span', [], [arizona_template:get(b, Bindings)]}; "
        "                _ -> <<>> "
        "            end"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<div><span>B</span></div>">>, render_markerless(Mod:render(#{f => true, b => ~"B"}))
    ).

%% `az-nodiff` on a bare element in a conditional branch reaches the templates
%% nested in it, exactly as on the same element wrapped in `?html`.
nodiff_bare_branch_inherited(Config) when is_list(Config) ->
    Section = <<"<section><i>B</i><li>1</li><li>2</li></section>">>,
    lists:foreach(
        fun({Name, Branch}) ->
            Mod = compile_module(
                "-module(" ++ Name ++
                    "). "
                    "-export([render/1]). "
                    "render(Bindings) -> "
                    "    arizona_template:html("
                    "        {'div', [], ["
                    "            case arizona_template:get(f, Bindings) of "
                    "                true -> " ++ Branch ++
                    "; "
                    "                _ -> <<>> "
                    "            end"
                    "        ]}"
                    "    ). "
            ),
            HTML = strip_nodiff_attr(
                iolist_to_binary(
                    arizona_render:render_to_iolist(
                        Mod:render(#{f => true, b => ~"B", xs => [1, 2]})
                    )
                )
            ),
            ?assertMatch({_, _}, binary:match(HTML, Section))
        end,
        [
            {"pt_nodiff_bare_branch", nodiff_section()},
            {"pt_nodiff_wrapped_branch", "arizona_template:html(" ++ nodiff_section() ++ ")"}
        ]
    ).

nodiff_section() ->
    "{'section', ['az-nodiff'], ["
    "    arizona_template:html({'i', [], [arizona_template:get(b, Bindings)]}), "
    "    arizona_template:each(fun(X) -> {'li', [], [X]} end, arizona_template:get(xs, Bindings))"
    "]}".

%% A slot holding an az-nodiff branch still switches branches: only that template
%% renders once, the slot itself keeps diffing.
nodiff_branch_swaps_away(Config) when is_list(Config) ->
    lists:foreach(
        fun({Name, Branch}) -> assert_nodiff_swap(Name, Branch) end,
        [
            {"pt_nodiff_swap_bare",
                "{'section', ['az-nodiff'], [arizona_template:get(b, Bindings)]}"},
            {"pt_nodiff_swap_wrapped",
                "arizona_template:html({'section', ['az-nodiff'], "
                "[arizona_template:get(b, Bindings)]})"}
        ]
    ).

assert_nodiff_swap(Name, Branch) ->
    Mod = compile_module(
        "-module(" ++ Name ++
            "). "
            "-export([render/1]). "
            "render(Bindings) -> "
            "    arizona_template:html("
            "        {'div', [], ["
            "            case arizona_template:get(f, Bindings) of "
            "                true -> " ++ Branch ++
            "; "
            "                _ -> {'p', [], [<<\"off\">>]} "
            "            end"
            "        ]}"
            "    ). "
    ),
    On = #{f => true, b => ~"B1"},
    {_, OnSnap, _} = arizona_render:render(Mod:render(On), #{}),
    %% Same branch, changed inner binding: rendered once, nothing to patch --
    %% whether or not the inner read is a dep of the slot (`f` changing to
    %% the same branch re-runs the slot and lands on the same template).
    ?assertMatch(
        {[], _, _},
        arizona_diff:diff(Mod:render(On#{b => ~"B2"}), OnSnap, #{}, #{b => true})
    ),
    {[], SameSnap, _} =
        arizona_diff:diff(Mod:render(On#{b => ~"B2"}), OnSnap, #{}, #{f => true}),
    ?assertEqual(maps:get(d, OnSnap), maps:get(d, SameSnap)),
    %% Away from the nodiff branch, then back to it.
    {Away, OffSnap, _} =
        arizona_diff:diff(Mod:render(On#{f => false}), OnSnap, #{}, #{f => true}),
    ?assertMatch([[?OP_TEXT, _, _]], Away),
    ?assertMatch(
        {[[?OP_TEXT, _, _]], _, _},
        arizona_diff:diff(Mod:render(On), OffSnap, #{}, #{f => true})
    ).

%% A slot that re-runs and lands on the same az-nodiff template keeps its first
%% render without evaluating the az-nodiff element again: a child view inside it gets no
%% `handle_update/3` (its DOM is never patched, so its state must not move past it),
%% and the old snapshot is carried as-is.
nodiff_branch_same_template_skips_eval(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_same_tmpl). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            case arizona_template:get(n, Bindings) > 0 of "
        "                true -> {'section', ['az-nodiff'], ["
        "                    arizona_template:stateful(arizona_counter, "
        "                        #{id => <<\"nd_c\">>, "
        "                          count => arizona_template:get(n, Bindings)}), "
        "                    arizona_template:each(fun(X) -> {'li', [], [X]} end, "
        "                        arizona_template:get(xs, Bindings))"
        "                ]}; "
        "                false -> <<\"off\">> "
        "            end"
        "        ]}"
        "    ). "
    ),
    B0 = #{n => 1, xs => [1, 2]},
    {_, Snap0, Views0} = arizona_render:render(Mod:render(B0), #{}),
    {Ops, Snap1, Views1} =
        arizona_diff:diff(Mod:render(B0#{n => 2, xs => [3]}), Snap0, Views0, #{n => true}),
    ?assertEqual([], Ops),
    ?assertEqual(maps:get(d, Snap0), maps:get(d, Snap1)),
    ?assertEqual(maps:get(~"nd_c", Views0), maps:get(~"nd_c", Views1)),
    %% Leaving the branch still patches the slot.
    ?assertMatch(
        {[[?OP_TEXT, _, <<"off">>]], _, _},
        arizona_diff:diff(Mod:render(B0#{n => 0}), Snap0, Views0, #{n => true})
    ).

%% An `?each` item's slot leaves an az-nodiff branch like a top-level one does.
nodiff_item_branch_swaps_away(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_item_swap). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [], [arizona_template:each(fun(X) -> "
        "            {'li', [], [case X of "
        "                1 -> {'b', ['az-nodiff'], [X]}; "
        "                _ -> {'i', [], [X]} "
        "            end]} end, arizona_template:get(xs, Bindings))]}"
        "    ). "
    ),
    {_, Snap0, V0} = arizona_render:render(Mod:render(#{xs => [1]}), #{}),
    {Ops, _, _} = arizona_diff:diff(Mod:render(#{xs => [2]}), Snap0, V0, #{xs => true}),
    ?assertMatch(
        [[?OP_LIST_PATCH, _, [[?OP_ITEM_PATCH, 0, [[?OP_TEXT, _, #{~"d" := [~"2"]}]]]]]], Ops
    ).

%% Two az-nodiff branches with the same statics are still different templates:
%% the slot switches between them (an az-nodiff element is named by the branch that produced
%% it, not only by its statics).
nodiff_same_statics_branches_swap(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_same_statics). "
        "-export([render/1, home/1, about/1]). "
        "home(_Bindings) -> arizona_template:html({'p', [], [<<\"Home\">>]}). "
        "about(_Bindings) -> arizona_template:html({'p', [], [<<\"About\">>]}). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            case arizona_template:get(lang, Bindings) of "
        "                en -> {'p', ['az-nodiff'], "
        "                    [<<\"Hi \">>, arizona_template:get(a, Bindings)]}; "
        "                pt -> {'p', ['az-nodiff'], "
        "                    [<<\"Hi \">>, arizona_template:get(b, Bindings)]} "
        "            end, "
        "            case arizona_template:get(tab, Bindings) of "
        "                home -> {'section', ['az-nodiff'], "
        "                    [arizona_template:stateless(fun home/1, #{})]}; "
        "                about -> {'section', ['az-nodiff'], "
        "                    [arizona_template:stateless(fun about/1, #{})]} "
        "            end"
        "        ]}"
        "    ). "
    ),
    B0 = #{lang => en, a => ~"A", b => ~"B", tab => home},
    {_, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    ?assertMatch(
        {[[?OP_TEXT, _, _]], _, _},
        arizona_diff:diff(Mod:render(B0#{lang => pt}), Snap0, V0, #{lang => true})
    ),
    {[[?OP_TEXT, _, Tab]], _, _} =
        arizona_diff:diff(Mod:render(B0#{tab => about}), Snap0, V0, #{tab => true}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(io_lib:format("~p", [Tab])), ~"About")).

%% A child view whose own root is az-nodiff keeps its state wherever its
%% descriptor is re-evaluated from -- a branch element, a `?stateless` body, a
%% plain-list item -- so its state never moves past a DOM no op touches.
nodiff_root_child_kept_wherever_evaluated(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_root_child_kept). "
        "-export([render/1, wrap/1]). "
        "wrap(Bindings) -> arizona_template:html({'span', [], ["
        "    arizona_template:stateful(arizona_no_diff_counter, "
        "        #{id => <<\"ndc_s\">>, count => arizona_template:get(c, Bindings)})]}). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'div', [], [<<\"x\">>, "
        "                    arizona_template:stateful(arizona_no_diff_counter, "
        "                        #{id => <<\"ndc_b\">>, "
        "                          count => arizona_template:get(c, Bindings)})]}; "
        "                false -> <<\"off\">> "
        "            end, "
        "            arizona_template:stateless(fun wrap/1, "
        "                #{c => arizona_template:get(c, Bindings)}), "
        "            [{'i', [], [arizona_template:stateful(arizona_no_diff_counter, "
        "                #{id => <<\"ndc_l\">>, count => arizona_template:get(c, Bindings)})]}]"
        "        ]}"
        "    ). "
    ),
    B0 = #{f => true, c => 1},
    {_, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    {Ops, _, V1} = arizona_diff:diff(Mod:render(B0#{c => 2}), Snap0, V0, #{c => true}),
    ?assertEqual([], Ops),
    [
        ?assertEqual(maps:get(Id, V0), maps:get(Id, V1))
     || Id <- [~"ndc_b", ~"ndc_s", ~"ndc_l"]
    ].

%% A slot holding a nodiff-root child view still leaves it when its value
%% changes, and the dep-aware and plain diffs agree.
nodiff_root_child_slot_leaves(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_root_child_leaves). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            case arizona_template:get(show, Bindings) of "
        "                true -> arizona_template:stateful(arizona_no_diff_counter, "
        "                    #{id => <<\"ndc\">>, count => arizona_template:get(c, Bindings)}); "
        "                false -> <<\"gone\">> "
        "            end"
        "        ]}"
        "    ). "
    ),
    B0 = #{show => true, c => 1},
    {_, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    New = Mod:render(B0#{show => false}),
    {Ops4, _, V4} = arizona_diff:diff(New, Snap0, V0, #{show => true}),
    {Ops2, _, V2} = arizona_diff:diff(New, Snap0, V0),
    ?assertMatch([[?OP_TEXT, _, <<"gone">>]], Ops4),
    ?assertEqual(Ops4, Ops2),
    ?assertNot(maps:is_key(~"ndc", V4)),
    ?assertNot(maps:is_key(~"ndc", V2)),
    %% Re-running into the same child keeps it without an op.
    {Same, _, VSame} =
        arizona_diff:diff(Mod:render(B0#{c => 2}), Snap0, V0, #{c => true}),
    ?assertEqual([], Same),
    ?assertEqual(maps:get(~"ndc", V0), maps:get(~"ndc", VSame)).

%% A child view's inner slot leaves an az-nodiff branch when the child re-renders
%% under its parent (the child-view path diffs its inner dynamics itself).
nodiff_child_view_branch_swaps_away(Config) when is_list(Config) ->
    pt_nodiff_child_view = compile_module(
        "-module(pt_nodiff_child_view). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(Bindings) -> {#{id => maps:get(id, Bindings), f => maps:get(f, Bindings)}, #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, arizona_template:get(id, Bindings)}], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'section', ['az-nodiff'], [<<\"on\">>]}; "
        "                false -> {'p', [], [<<\"off\">>]} "
        "            end"
        "        ]}"
        "    ). "
    ),
    Parent = compile_module(
        "-module(pt_nodiff_child_parent). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'main', [], [arizona_template:stateful(pt_nodiff_child_view, "
        "        #{id => <<\"cv\">>, f => arizona_template:get(f, Bindings)})]}). "
    ),
    {_, Snap0, V0} = arizona_render:render(Parent:render(#{f => true}), #{}),
    {Ops, _, _} = arizona_diff:diff(Parent:render(#{f => false}), Snap0, V0, #{f => true}),
    ?assertMatch([[~"cv", [[?OP_TEXT, _, _]]]], Ops).

%% A `?local` inside an az-nodiff branch is never patched while the branch stays,
%% and a swap away and back re-renders the branch with its initial value.
nodiff_branch_local_across_swap(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_local_swap). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'section', ['az-nodiff'], ["
        "                    {'span', [], [arizona_template:local(<<\"k\">>, <<\"init\">>)]}]}; "
        "                false -> <<\"off\">> "
        "            end"
        "        ]}"
        "    ). "
    ),
    {_, OnSnap, V0} = arizona_render:render(Mod:render(#{f => true}), #{}),
    ?assertMatch({[], _, _}, arizona_diff:diff(Mod:render(#{f => true}), OnSnap, V0, #{f => true})),
    {[[?OP_TEXT, _, <<"off">>]], OffSnap, V1} =
        arizona_diff:diff(Mod:render(#{f => false}), OnSnap, V0, #{f => true}),
    ?assertMatch(
        {[[?OP_TEXT, _, #{~"d" := [_, ~"init"]}]], _, _},
        arizona_diff:diff(Mod:render(#{f => true}), OffSnap, V1, #{f => true})
    ).

nodiff_inherited_nested_html(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            arizona_template:html({'em', [], [arizona_template:get(a, Bindings)]})"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(<<"<div><em>A</em></div>">>, render_markerless(Mod:render(#{a => ~"A"}))).

nodiff_inherited_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun(I) -> {'li', [], [I]} end, "
        "                arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<ul><li>1</li><li>2</li></ul>">>, render_markerless(Mod:render(#{items => [1, 2]}))
    ).

nodiff_inherited_each_in_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_each_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun(I) -> "
        "                {'li', [], [arizona_template:each(fun(J) -> {'b', [], [J]} end, I)]} "
        "            end, arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<ul><li><b>1</b><b>2</b></li></ul>">>,
        render_markerless(Mod:render(#{items => [[1, 2]]}))
    ).

nodiff_inherited_hoisted_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_hoisted). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Rows = arizona_template:each(fun(I) -> {'li', [], [I]} end, "
        "        arizona_template:get(items, Bindings)), "
        "    arizona_template:html({'ul', ['az-nodiff'], [Rows]}). "
    ),
    ?assertEqual(
        <<"<ul><li>1</li><li>2</li></ul>">>, render_markerless(Mod:render(#{items => [1, 2]}))
    ).

nodiff_inherited_helper_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_helper). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], [row(arizona_template:get(a, Bindings))]}"
        "    ). "
        "row(X) -> {'li', [], [arizona_template:each(fun(I) -> {'i', [], [I]} end, [X])]}. "
    ),
    ?assertEqual(<<"<ul><li><i>A</i></li></ul>">>, render_markerless(Mod:render(#{a => ~"A"}))).

nodiff_inherited_named_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_named). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun row/1, arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
        "row(I) -> {'li', [], [case I of 1 -> {'b', [], [I]}; _ -> {'i', [], [I]} end]}. "
    ),
    ?assertEqual(
        <<"<ul><li><b>1</b></li><li><i>2</i></li></ul>">>,
        render_markerless(Mod:render(#{items => [1, 2]}))
    ).

%% An ?each item body is a compile unit of its own, so az-nodiff on the item
%% element alone reaches the templates nested in the item.
nodiff_each_item_inherited(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_item_inh). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'table', [], ["
        "            arizona_template:each(fun({Id, Msg}) -> "
        "                {'tr', ['az-nodiff'], ["
        "                    {'td', [], [Id]}, "
        "                    case Msg of <<>> -> <<>>; _ -> {'td', [], [Msg]} end"
        "                ]} "
        "            end, arizona_template:get(rows, Bindings))"
        "        ]}"
        "    ). "
    ),
    %% The table itself is diffable, so only the item's own markup is markerless.
    HTML = strip_nodiff_attr(
        iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{rows => [{~"1", ~"a&b"}]})))
    ),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<tr><td>1</td><td>a&amp;b</td></tr>">>)).

%% A layout (?inner_content) is auto-nodiff, and that reaches its nested ?each.
nodiff_layout_inherited_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_layout_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'body', [], ["
        "            {'nav', [], [arizona_template:each(fun(L) -> {'a', [], [L]} end, "
        "                arizona_template:get(links, Bindings))]}, "
        "            az:inner_content(Bindings)"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<body><nav><a>x</a></nav>page</body>">>,
        render_markerless(Mod:render(#{links => [~"x"], inner_content => ~"page"}))
    ).

%% A ?stateful child is its own live view and diffs itself: a template handed to
%% it in props is NOT nodiff, even under a nodiff root.
nodiff_stateful_props_not_inherited(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_stateful_props). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            arizona_template:stateful(child_mod, #{"
        "                body => arizona_template:html("
        "                    {'p', [], [arizona_template:get(a, Bindings)]})"
        "            })"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{a => ~"A"}),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {attr, ~"az-nodiff", _}, _}, {undefined, Fun, _Loc}] = maps:get(d, T),
    #{stateful := child_mod, props := #{body := Body}} = Fun(),
    ?assertNot(maps:is_key(diff, Body)),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Body)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<" az=\"">>)),
    ?assertNotEqual(nomatch, binary:match(HTML, <<"<!--az:">>)).

%% A ?local in a template nested under az-nodiff keeps its own address: each item's
%% element carries an `az` and the local slot's marker, nothing else does.
nodiff_inherited_local(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun(I) -> "
        "                {'li', [], [I, arizona_template:local(<<\"k\">>, <<\"x\">>)]} "
        "            end, arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [1, 2]}))),
    ?assertEqual(#{~"c" => #{~"1" => ~"k"}}, decode_local_descriptor(HTML)),
    ?assertEqual(2, length(binary:matches(HTML, <<" az=\"">>))),
    ?assertEqual(2, length(binary:matches(HTML, <<":1-->x<!--/az-->">>))),
    ?assertEqual(2, length(binary:matches(HTML, <<"<!--az:">>))).

%% A layout is nodiff, and a ?local in its conditional chrome renders exactly as it
%% would without inheritance: the client reaches it with `set_all`.
nodiff_layout_branch_local(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_layout_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'body', [], ["
        "            case arizona_template:get(menu, Bindings) of "
        "                true -> {'nav', [], [{'span', [], "
        "                    [arizona_template:local(<<\"open\">>, <<\"no\">>)]}]}; "
        "                false -> <<>> "
        "            end, "
        "            az:inner_content(Bindings)"
        "        ]}"
        "    ). "
    ),
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:render(#{menu => true, inner_content => ~"page"}))
    ),
    ?assertEqual(#{~"c" => #{~"0" => ~"open"}}, decode_local_descriptor(HTML)),
    ?assertEqual(1, length(binary:matches(HTML, <<" az=\"">>))),
    ?assertMatch({_, _}, binary:match(HTML, <<"-->no<!--/az--></span></nav>page</body>">>)).

%% Attribute locals are written by attribute name, so a nodiff element holding only
%% those carries no `az` at all.
nodiff_attr_local_no_az(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_attr_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            {'dialog', ["
        "                {open, arizona_template:local(<<\"o\">>, false)},"
        "                {class, [<<\"d \">>, arizona_template:local(<<\"c\">>, <<\"x\">>)]}"
        "            ], [arizona_template:get(a, Bindings)]}"
        "        ]}"
        "    ). "
    ),
    HTML = render_markerless(Mod:render(#{a => ~"A"})),
    ?assertMatch(
        #{~"a" := #{~"open" := ~"o", ~"class" := ~"c"}, ~"ap" := #{~"class" := [~"d ", ~""]}},
        decode_local_descriptor(HTML)
    ),
    ?assertMatch({_, _}, binary:match(HTML, <<" class=\"d x\">A</dialog></div>">>)).

%% Every dynamic child counts a slot even where only the local is marked, so the
%% descriptor's slot index and the marker agree.
nodiff_local_slot_numbering(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_local_slots). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            {'p', [], [arizona_template:get(a, Bindings), <<\" / \">>, "
        "                arizona_template:local(<<\"m\">>, <<\"M\">>), "
        "                arizona_template:get(a, Bindings)]}"
        "        ]}"
        "    ). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{a => ~"A"}))),
    ?assertEqual(#{~"c" => #{~"1" => ~"m"}}, decode_local_descriptor(HTML)),
    ?assertEqual(1, length(binary:matches(HTML, <<"<!--az:">>))),
    ?assertMatch({_, _}, binary:match(HTML, <<">A / <!--az:">>)),
    ?assertMatch({_, _}, binary:match(HTML, <<":1-->M<!--/az-->A</p>">>)).

%% A fun is called later by whichever view receives it -- here a live child, via a
%% ?stateless prop. A template it builds must stay diffable, or the child's slot
%% would hold a `diff => false` value the differ skips forever.
nodiff_fun_prop_not_inherited(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_fun_prop). "
        "-export([marked/1, fwd/1, child/1]). "
        "marked(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            arizona_template:stateless(fun fwd/1, #{mk => fun(N) -> "
        "                arizona_template:html({'b', [], [N]}) end})"
        "        ]}"
        "    ). "
        "fwd(Bindings) -> "
        "    put(pt_nodiff_fun_prop, arizona_template:get(mk, Bindings)), "
        "    arizona_template:html({'span', [], []}). "
        "child(Bindings) -> "
        "    arizona_template:html({'p', [], "
        "        [(arizona_template:get(mk, Bindings))(arizona_template:get(n, Bindings))]}). "
    ),
    _ = arizona_render:render_to_iolist(Mod:marked(#{})),
    Mk = erase(pt_nodiff_fun_prop),
    ?assertNot(maps:is_key(diff, Mk(0))),
    {_HTML, Snap, Views} = arizona_render:render(Mod:child(#{mk => Mk, n => 0}), #{}),
    ?assertMatch(
        {[[_, _, <<"1">>]], _, _},
        arizona_diff:diff(Mod:child(#{mk => Mk, n => 1}), Snap, Views, #{n => true})
    ).

%% The same as above for a named fun, which is its own AST node.
nodiff_named_fun_prop_not_inherited(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_named_fun_prop). "
        "-export([marked/1, fwd/1, child/1]). "
        "marked(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            arizona_template:stateless(fun fwd/1, #{mk => fun Self(N) -> "
        "                arizona_template:html({'b', [], [N]}) end})"
        "        ]}"
        "    ). "
        "fwd(Bindings) -> "
        "    put(pt_nodiff_named_fun_prop, arizona_template:get(mk, Bindings)), "
        "    arizona_template:html({'span', [], []}). "
        "child(Bindings) -> "
        "    arizona_template:html({'p', [], "
        "        [(arizona_template:get(mk, Bindings))(arizona_template:get(n, Bindings))]}). "
    ),
    _ = arizona_render:render_to_iolist(Mod:marked(#{})),
    Mk = erase(pt_nodiff_named_fun_prop),
    ?assertNot(maps:is_key(diff, Mk(0))),
    {_HTML, Snap, Views} = arizona_render:render(Mod:child(#{mk => Mk, n => 0}), #{}),
    ?assertMatch(
        {[[_, _, <<"1">>]], _, _},
        arizona_diff:diff(Mod:child(#{mk => Mk, n => 1}), Snap, Views, #{n => true})
    ).

%% A named ?each callback inherits the helper bodies it splices in, as an inline one does.
nodiff_inherited_named_each_helper(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_named_helper). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun row/1, arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
        "row(I) -> {'li', [], [cell(I)]}. "
        "cell(X) -> {'span', [], ["
        "    arizona_template:html({'i', [], [X]}), "
        "    arizona_template:each(fun(J) -> {'u', [], [J]} end, [X])"
        "]}. "
    ),
    ?assertEqual(
        <<"<ul><li><span><i>a</i><u>a</u></span></li></ul>">>,
        render_markerless(Mod:render(#{items => [~"a"]}))
    ).

%% A hoisted ?each source renders as item content, so it is inherited like an inline one.
nodiff_inherited_hoisted_source(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_source). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Rows = [arizona_template:html({'b', [], [X]}) "
        "        || X <- arizona_template:get(items, Bindings)], "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun(R) -> {'li', [], [R]} end, Rows)"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<ul><li><b>a</b></li></ul>">>, render_markerless(Mod:render(#{items => [~"a"]}))
    ).

%% A ?stateful child in a nodiff ?each item still mounts as its own live view and
%% keeps its own markers.
nodiff_each_stateful_item(Config) when is_list(Config) ->
    _ = compile_module(
        "-module(pt_nodiff_each_child). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {#{id => maps:get(id, B), v => 0}, #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html({'span', [{id, arizona_template:get(id, Bindings)}], "
        "        [arizona_template:get(v, Bindings)]}). "
    ),
    Mod = compile_module(
        "-module(pt_nodiff_each_stateful). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun(I) -> "
        "                {'li', [], [arizona_template:stateful(pt_nodiff_each_child, #{id => I})]}"
        "            end, arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
    ),
    {HTML, _Snap, Views} = arizona_render:render(Mod:render(#{items => [~"a", ~"b"]}), #{}),
    ?assertEqual([~"a", ~"b"], lists:sort(maps:keys(Views))),
    ?assertMatch(
        {match, _},
        re:run(
            strip_nodiff_attr(iolist_to_binary(HTML)),
            <<"^<ul><li><span az=\"[^\"]+\" az-view id=\"a\">",
                "<!--az:[^>]+-->0<!--/az--></span></li>">>
        )
    ).

%% Stream items under az-nodiff keep their `az-key` and carry no `az`.
nodiff_stream_az_key(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_stream_key). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff'], ["
        "            arizona_template:each(fun(I, K) -> {'li', [{az_key, K}], [I]} end, "
        "                arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
    ),
    S = arizona_stream:new(fun(X) -> X end, [~"x", ~"y"]),
    ?assertEqual(
        <<"<ul><li az-key=\"x\">x</li><li az-key=\"y\">y</li></ul>">>,
        render_markerless(Mod:render(#{items => S}))
    ).

%% Inheritance is target-neutral: a nested ?native template loses its `az` too.
nodiff_inherited_native(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native("
        "        {'Column', ['az-nodiff'], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> "
        "                    {'Text', [{color, arizona_template:get(c, Bindings)}], [<<\"x\">>]}; "
        "                _ -> <<>> "
        "            end"
        "        ]}"
        "    ). "
    ),
    Out = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{f => true, c => ~"red"}))),
    ?assertNotEqual(nomatch, binary:match(Out, <<"\"type\":\"Text\"">>)),
    ?assertEqual(nomatch, binary:match(Out, <<"\"az\"">>)).

%% Raw-text elements in an inherited branch stay markerless and keep their own
%% escaping (textarea) or breakout neutralization (script).
nodiff_inherited_raw_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_inh_raw_text). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'textarea', [], [arizona_template:get(c, Bindings)]}; "
        "                _ -> <<>> "
        "            end, "
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'script', [], "
        "                    [arizona_template:raw(arizona_template:get(c, Bindings))]}; "
        "                _ -> <<>> "
        "            end"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<div><textarea>&lt;/script&gt;&amp;</textarea><script><\\/script>&</script></div>">>,
        render_markerless(Mod:render(#{f => true, c => ~"</script>&"}))
    ).

%% A live root carrying az-nodiff inherits into material spliced into it, like any
%% other nodiff template.
nodiff_live_root_hoisted_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_live_hoisted). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(Bindings) -> {Bindings, #{}}. "
        "render(Bindings) -> "
        "    Rows = arizona_template:each(fun(I) -> {'li', [], [I]} end, "
        "        arizona_template:get(items, Bindings)), "
        "    arizona_template:html("
        "        {'ul', ['az-nodiff', {id, arizona_template:get(id, Bindings)}], [Rows]}"
        "    ). "
    ),
    ?assertEqual(
        <<"<ul az-view id=\"lv\"><li>1</li><li>2</li></ul>">>,
        render_markerless(Mod:render(#{id => ~"lv", items => [1, 2]}))
    ).

%% An inherited constructor call that no compile path reached is a compile error,
%% never an `undef` at render.
nodiff_call_not_compiled_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_nodiff_sentinel). "
        "-export([f/0]). "
        "f() -> '$arizona_nodiff':nope(). ",
        fun(R) -> R =:= nodiff_call_not_compiled end
    ).

%% Without the directive nested templates keep every marker.
nodiff_absent_keeps_nested_markers(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_absent_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [], ["
        "            case arizona_template:get(f, Bindings) of "
        "                true -> {'li', [], [arizona_template:get(a, Bindings)]}; "
        "                _ -> <<>> "
        "            end, "
        "            arizona_template:each(fun(I) -> {'li', [], [I]} end, "
        "                arizona_template:get(items, Bindings))"
        "        ]}"
        "    ). "
    ),
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:render(#{f => true, a => ~"A", items => [1]}))
    ),
    %% ul, branch li and each li each carry an az; four slots carry comment markers.
    ?assertEqual(3, length(binary:matches(HTML, <<" az=\"">>))),
    ?assertEqual(4, length(binary:matches(HTML, <<"<!--az:">>))),
    ?assertEqual(4, length(binary:matches(HTML, <<"<!--/az-->">>))).

%% az-nodiff is only honored on a template's top-level element (directives are
%% stripped there). On a NESTED element it is neither stripped nor honored -- left as
%% an ordinary boolean attr it would leak the reserved `az-nodiff` into the DOM while
%% the element stayed diffable. It must be a clear compile error, not silently ignored.
nodiff_nested_element_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_nodiff_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            {'span', ['az-nodiff'], [arizona_template:get(x, Bindings)]}"
        "        ]}"
        "    ). ",
        fun(R) -> R =:= nested_nodiff end
    ).

%% Test 67: mixed attr forms -- atom, binary, {k,true}, {k,false}, {k,v} on one element.
mixed_attr_forms(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_mixed_attrs). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'input', [disabled, <<\"required\">>, {hidden, true}, "
        "                   {autofocus, false}, {class, <<\"field\">>}], []}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    ?assertEqual(<<"<input disabled required hidden class=\"field\" />">>, Static).

%% ==========================================================================
%% Layout tests
%% ==========================================================================

%% Test 68: Layout with single element -- no markers, no az attrs, no f key.
layout_single_element(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_elem). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', ['az-nodiff'], [maps:get(text, Bindings, <<\"hi\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{text => <<"hello">>}),
    ?assertEqual([<<"<p">>, <<">">>, <<"</p>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [
        {undefined, {attr, <<"az-nodiff">>, _}, _},
        {undefined, {esc, Fun}, {pt_layout_elem, 1}}
    ] = maps:get(d, T),
    ?assert(is_function(Fun, 0)),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 69: Layout with mixed list -- binary + element with az-nodiff.
layout_mixed_list(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"<!DOCTYPE html>\">>, "
        "        {'html', ['az-nodiff'], ["
        "            {'head', [], [{'title', [], [maps:get(title, Bindings, <<\"Test\">>)]}]}"
        "        ]}"
        "    ]). "
    ),
    T = Mod:render(#{title => <<"Hello">>}),
    Statics = maps:get(s, T),
    ?assertEqual([<<"<!DOCTYPE html><html><head><title>">>, <<"</title></head></html>">>], Statics),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {esc, Fun}, {pt_layout_mixed, 1}}] = maps:get(d, T),
    ?assertEqual(<<"Hello">>, Fun()).

%% Test 70: Layout with dynamic attribute -- plain fun, no az attr.
layout_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_dyn_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff', {class, maps:get(cls, Bindings, <<\"x\">>)}], [<<\"ok\">>]}"
        "    ). "
    ),
    T = Mod:render(#{cls => <<"active">>}),
    ?assertEqual([<<"<div">>, <<>>, <<">ok</div>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [
        {undefined, {attr, <<"az-nodiff">>, _}, _},
        {undefined, {attr, <<"class">>, Fun}, {pt_layout_dyn_attr, 1}}
    ] =
        maps:get(d, T),
    ?assert(is_function(Fun, 0)),
    ?assertEqual(<<"active">>, Fun()).

%% Test 71: render_to_iolist/1 integration -- full render produces clean HTML.
layout_render_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"<!DOCTYPE html>\">>, "
        "        {'html', ['az-nodiff'], ["
        "            {'head', [], [{'title', [], [maps:get(title, Bindings)]}]}, "
        "            {'body', [], [maps:get(content, Bindings)]}"
        "        ]}"
        "    ]). "
    ),
    T = Mod:render(#{title => <<"My Page">>, content => <<"Hello World">>}),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertEqual(
        <<
            "<!DOCTYPE html><html><head><title>My Page</title></head>"
            "<body>Hello World</body></html>"
        >>,
        HTML
    ).

%% Test 72: Layout with static-only content -- has f key, diff => false.
layout_static_only(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_static). "
        "-export([render/0]). "
        "render() -> "
        "    arizona_template:html({'p', ['az-nodiff', {class, <<\"box\">>}], [<<\"Hello\">>]}). "
    ),
    T = Mod:render(),
    ?assertEqual([<<"<p">>, <<" class=\"box\">Hello</p>">>], maps:get(s, T)),
    ?assertMatch([{undefined, {attr, <<"az-nodiff">>, _}, _}], maps:get(d, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)).

%% Test 73: Layout with multiple dynamic children -- no markers, slot doesn't increment.
layout_multiple_dynamic_children(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_multi_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'p', ['az-nodiff'], ["
        "            maps:get(a, Bindings), "
        "            <<\" and \">>, "
        "            maps:get(b, Bindings)"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"X">>, b => <<"Y">>}),
    ?assertEqual([<<"<p">>, <<">">>, <<" and ">>, <<"</p>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [
        {undefined, {attr, <<"az-nodiff">>, _}, _},
        {undefined, {esc, FunA}, {pt_layout_multi_dyn, 1}},
        {undefined, {esc, FunB}, {pt_layout_multi_dyn, 1}}
    ] = maps:get(d, T),
    ?assert(is_function(FunA, 0)),
    ?assert(is_function(FunB, 0)),
    ?assertEqual(<<"X">>, FunA()),
    ?assertEqual(<<"Y">>, FunB()),
    %% No markers in statics
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"<!--az:">>)).

%% Test 74: Layout with void element + dynamic attr -- no az attr.
layout_void_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_void_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'input', ['az-nodiff', {value, maps:get(v, Bindings)}], []}"
        "    ). "
    ),
    T = Mod:render(#{v => <<"hello">>}),
    ?assertEqual([<<"<input">>, <<>>, <<" />">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [
        {undefined, {attr, <<"az-nodiff">>, _}, _},
        {undefined, {attr, <<"value">>, Fun}, {pt_layout_void_dyn, 1}}
    ] =
        maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()),
    %% No az attr in statics
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az=\"">>)).

%% Test 75: html with a single dynamic expression -- now uses standard html
%% path, with the root slot marker-anchored in the statics.
layout_bare_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_bare_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(maps:get(content, Bindings)). "
    ),
    T = Mod:render(#{content => <<"hi">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        [<<"<!--az:", Fp/binary, "-0-->">>, <<"<!--/az-->">>], maps:get(s, T)
    ),
    [{_Az, {esc, Fun}, {pt_layout_bare_dyn, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hi">>, Fun()).

%% Test 76: html with mixed list including a dynamic at top level (no nodiff).
layout_mixed_dynamic_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_mixed_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"prefix\">>, "
        "        maps:get(x, Bindings), "
        "        <<\"suffix\">>"
        "    ]). "
    ),
    T = Mod:render(#{x => <<"middle">>}),
    Fp = maps:get(f, T),
    %% The bare dynamic is wrapped in text-slot markers (patchable in place); with
    %% no element in the fragment there is no collision, but markers are still the
    %% correct, consistent behavior for a diffable slot.
    ?assertEqual(
        scope_s(Fp, [<<"prefix<!--az:0-->">>, <<"<!--/az-->suffix">>]),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_layout_mixed_dyn, 1}}] = maps:get(d, T),
    ?assertEqual(<<"middle">>, Fun()),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertEqual(<<"prefix<!--az:", Fp/binary, "-0-->middle<!--/az-->suffix">>, HTML).

%% Test 77: Layout nested elements -- no az attrs at any depth.
layout_nested_no_az(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], ["
        "            {'section', [], ["
        "                {'p', [], [maps:get(a, Bindings)]}"
        "            ]},"
        "            {'p', [], [maps:get(b, Bindings)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"deep">>, b => <<"shallow">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az=\"">>)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"<!--az:">>)),
    HTML = strip_nodiff_attr(iolist_to_binary(arizona_render:render_to_iolist(T))),
    ?assertEqual(<<"<div><section><p>deep</p></section><p>shallow</p></div>">>, HTML).

%% Test 77b: Mixed list with az-nodiff -- all dynamics get undefined Az
%% because directives are pre-scanned before compilation.
layout_mixed_with_bare_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_mixed_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"prefix\">>, "
        "        maps:get(x, Bindings), "
        "        {'p', ['az-nodiff'], [maps:get(y, Bindings)]}"
        "    ]). "
    ),
    T = Mod:render(#{x => <<"mid">>, y => <<"end">>}),
    ?assertEqual(false, maps:get(diff, T)),
    ?assert(maps:is_key(f, T)),
    %% Both dynamics get undefined Az -- directives pre-scanned.
    [
        {undefined, {esc, FunX}, {pt_layout_mixed_bare, 1}},
        {undefined, {esc, FunY}, {pt_layout_mixed_bare, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"mid">>, FunX()),
    ?assertEqual(<<"end">>, FunY()),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertEqual(<<"prefixmid<p>end</p>">>, HTML),
    %% No markers or az attrs anywhere
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az=\"">>)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"<!--az:">>)).

%% Test 77c: Mixed list without az-nodiff -- element dynamics get markers.
mixed_list_no_nodiff(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_mixed_no_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"<!DOCTYPE html>\">>, "
        "        {'div', [], [maps:get(x, Bindings)]}"
        "    ]). "
    ),
    T = Mod:render(#{x => <<"hello">>}),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:is_key(diff, T)),
    %% Dynamic inside element gets a proper binary Az (scoped)
    [{Az, {esc, Fun}, {pt_mixed_no_nodiff, 1}}] = maps:get(d, T),
    ?assert(is_binary(Az)),
    ?assertEqual(<<"hello">>, Fun()),
    %% Element has az attr and markers
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az=\"">>)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"<!--az:">>)).

%% Test 77d: ?inner_content at top level of a mixed list -- auto-detected
%% as a layout, gets `diff => false` without explicit `az-nodiff`.
auto_nodiff_inner_content_top_level(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_auto_nodiff_top). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        <<\"<outer>\">>, "
        "        az:inner_content(Bindings), "
        "        <<\"</outer>\">>"
        "    ]). "
    ),
    T = Mod:render(#{inner_content => <<"hi">>}),
    ?assertEqual(false, maps:get(diff, T)),
    %% Dynamics carry undefined Az (nodiff -- never used as op target)
    [{undefined, Fun, {pt_auto_nodiff_top, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hi">>, Fun()).

%% Test 77e: ?inner_content nested inside an element's children -- the
%% recursive AST walker still finds it and auto-marks the template.
auto_nodiff_inner_content_nested(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_auto_nodiff_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'html', [], ["
        "            {'body', [], [az:inner_content(Bindings)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{inner_content => <<"page">>}),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, Fun, {pt_auto_nodiff_nested, 1}}] = maps:get(d, T),
    ?assertEqual(<<"page">>, Fun()).

%% Test 77f: A mixed list shaped like a layout but without ?inner_content
%% (e.g. `[{p, [], []}, ~"foo", {p, [], []}]`) is NOT auto-marked. The
%% template emits no `diff` key and dynamics get normal binary Az targets.
auto_nodiff_no_inner_content_no_diff_key(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_auto_nodiff_absent). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(["
        "        {'p', [], []}, "
        "        <<\"foo\">>, "
        "        {'p', [], [maps:get(x, Bindings)]}"
        "    ]). "
    ),
    T = Mod:render(#{x => <<"hello">>}),
    ?assertNot(maps:is_key(diff, T)),
    [{Az, _Fun, {pt_auto_nodiff_absent, _}}] = maps:get(d, T),
    ?assert(is_binary(Az)).

%% ==========================================================================
%% UTF-8 tests
%% ==========================================================================

%% Test 78: Static child with multi-byte Unicode codepoints (emoji).
utf8_static_child_emoji(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_utf8_emoji). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{class, <<\"operator\">>}], [<<\"\\x{1F9D1}\\x{200D}\\x{1F3ED}\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    Expected = <<"<div class=\"operator\">", 16#1F9D1/utf8, 16#200D/utf8, 16#1F3ED/utf8, "</div>">>,
    ?assertEqual(Expected, Static).

%% Test 79: Static child with cactus emoji (literal UTF-8 in source).
utf8_cactus_emoji(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_utf8_cactus). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [<<\"🌵\"/utf8>>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    Expected = <<"<p>🌵</p>"/utf8>>,
    ?assertEqual(Expected, Static).

%% Test 79: Static attribute value with Unicode emoji.
utf8_static_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_utf8_attr). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'span', [{title, <<\"\\x{2764}\">>}], [<<\"love\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    Expected = <<"<span title=\"", 16#2764/utf8, "\">love</span>">>,
    ?assertEqual(Expected, Static).

%% Test 80: Mixed ASCII and multi-byte Unicode in static child.
utf8_mixed_ascii(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_utf8_mixed). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [<<\"Hello \\x{1F30D}\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    [Static] = maps:get(s, T),
    Expected = <<"<p>Hello ", 16#1F30D/utf8, "</p>">>,
    ?assertEqual(Expected, Static).

%% Test 81: UTF-8 emoji in template/2 (each) static content.
utf8_each_static(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_utf8_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(_Item) -> "
        "        {'li', [], [<<\"\\x{2705} done\">>]} "
        "    end, arizona_template:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [a]}),
    Tmpl = maps:get(template, Result),
    [Static] = maps:get(s, Tmpl),
    Expected = <<"<li>", 16#2705/utf8, " done</li>">>,
    ?assertEqual(Expected, Static).

%% Test 82: UTF-8 render integration -- full render produces valid UTF-8 HTML.
utf8_render_integration(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_utf8_render). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], [<<\"\\x{1F9D1}\\x{200D}\\x{1F3ED} \">>,"
        "         arizona_template:get(name, Bindings, <<\"worker\">>)]}"
        "    ). "
    ),
    Tmpl = Mod:render(#{name => <<"Alice">>}),
    Fp = maps:get(f, Tmpl),
    {HTML, _Snap} = arizona_render:render(Tmpl),
    Bin = iolist_to_binary(HTML),
    Expected = iolist_to_binary(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\">", 16#1F9D1/utf8, 16#200D/utf8, 16#1F3ED/utf8,
                    " <!--az:0-->Alice<!--/az--></div>">>
            ]
        )
    ),
    ?assertEqual(Expected, Bin).

%% Test 83: iolist dynamic child -- render and diff.
iolist_dynamic_child(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_iolist_child). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], [arizona_template:get(content, Bindings, [<<\"a\">>, <<\"b\">>])]}"
        "    ). "
    ),
    Tmpl1 = Mod:render(#{content => [<<"foo">>, [<<"bar">>, <<"baz">>]]}),
    Fp = maps:get(f, Tmpl1),
    Az0 = <<Fp/binary, "-0">>,
    {HTML, Snap} = arizona_render:render(Tmpl1),
    ExpectedHTML = iolist_to_binary(
        scope_s(
            Fp,
            [<<"<div az=\"0\"><!--az:0-->foobarbaz<!--/az--></div>">>]
        )
    ),
    ?assertEqual(ExpectedHTML, iolist_to_binary(HTML)),
    %% Diff produces correct op
    Tmpl2 = Mod:render(#{content => [<<"x">>, <<"y">>]}),
    {Ops, _} = arizona_diff:diff(Tmpl2, Snap),
    ?assertEqual([[0, Az0, <<"xy">>]], Ops).

%% Test 84: Void element -- 2-element tuple (no children).
void_two_element_tuple(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_void_2elem). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'br', []}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<br />">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% Test 85: Void element -- 2-element tuple with dynamic attr.
void_two_element_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_void_2elem_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'input', [{type, <<\"text\">>}, "
        "                   {value, arizona_template:get(val, Bindings, <<>>)}]}"
        "    ). "
    ),
    T = Mod:render(#{val => <<"hi">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<input az=\"0\" type=\"text\"">>,
                <<" />">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {attr, <<"value">>, Fun}, {pt_void_2elem_dyn, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hi">>, Fun()).

%% Test 86: Non-void element -- single expression as third element.
single_expr_child(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_single_expr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'span', [], arizona_template:get(foo, Bindings, <<\"bar\">>)}"
        "    ). "
    ),
    T = Mod:render(#{foo => <<"baz">>}),
    Fp = maps:get(f, T),
    Az0 = <<Fp/binary, "-0">>,
    {HTML, Snap} = arizona_render:render(T),
    ExpectedHTML = iolist_to_binary(
        scope_s(
            Fp,
            [<<"<span az=\"0\"><!--az:0-->baz<!--/az--></span>">>]
        )
    ),
    ?assertEqual(ExpectedHTML, iolist_to_binary(HTML)),
    %% Diff works
    T2 = Mod:render(#{foo => <<"qux">>}),
    {Ops, _} = arizona_diff:diff(T2, Snap),
    ?assertEqual([[0, Az0, <<"qux">>]], Ops).

%% Test 87: to_bin crashes on non-renderable values.
to_bin_crash(Config) when is_list(Config) ->
    ?assertError({bad_template_value, _}, arizona_template:to_bin(#{a => 1})),
    ?assertError({bad_template_value, _}, arizona_template:to_bin({1, 2})),
    ?assertError({bad_template_value, _}, arizona_template:to_bin(make_ref())).

%% Test 88: Void element with static children raises compile-time error.
void_with_static_children(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_void_static). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'br', [], [<<\"text\">>]}). ",
        fun
            ({void_with_children, br}) -> true;
            (_) -> false
        end
    ).

%% Test 89: Void element with dynamic children raises compile-time error.
void_with_dynamic_children(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_void_dynamic). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'hr', [], [arizona_template:get(x, Bindings)]}). ",
        fun
            ({void_with_children, hr}) -> true;
            (_) -> false
        end
    ).

%% Test 90: Invalid attribute form raises compile-time error.
invalid_attribute(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [123], []}). ",
        fun
            (invalid_attribute) -> true;
            (_) -> false
        end
    ).

%% Test 91: format_error produces readable messages.
format_error(Config) when is_list(Config) ->
    Msg1 = arizona_parse_transform:format_error({void_with_children, br}),
    ?assert(lists:prefix("void element 'br'", Msg1)),
    Msg2 = arizona_parse_transform:format_error(invalid_attribute),
    ?assert(lists:prefix("invalid attribute form", Msg2)),
    Msg3 = arizona_parse_transform:format_error(invalid_element),
    ?assert(lists:prefix("invalid element form", Msg3)),
    Msg4 = arizona_parse_transform:format_error(invalid_each_fun),
    ?assert(lists:prefix("each/2 expects", Msg4)),
    Msg5 = arizona_parse_transform:format_error(tracked_get_on_non_bindings_map),
    ?assert(lists:prefix("arizona_template:get/get_lazy", Msg5)),
    ?assertNotEqual(nomatch, string:find(Msg5, "maps:get/2")).

%% Test 92: Void element with single-expression child form.
void_with_single_expr_child(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_void_single_expr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'br', [], arizona_template:get(x, Bindings)}). ",
        fun
            ({void_with_children, br}) -> true;
            (_) -> false
        end
    ).

%% Test 93: Void element as nested child raises compile-time error.
void_nested_child_with_children(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_void_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], [{'img', [], [<<\"oops\">>]}]}"
        "    ). ",
        fun
            ({void_with_children, img}) -> true;
            (_) -> false
        end
    ).

%% Test 94: Void element inside each template raises compile-time error.
void_in_each_with_children(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_void_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each("
        "        fun(Item) -> {'input', [], [<<\"bad\">>]} end,"
        "        arizona_template:get(items, Bindings, [])"
        "    ). ",
        fun
            ({void_with_children, input}) -> true;
            (_) -> false
        end
    ).

%% Test 95: Invalid attribute on a nested child element.
invalid_attribute_nested(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_attr_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], [{'span', [123], [<<\"text\">>]}]}"
        "    ). ",
        fun
            (invalid_attribute) -> true;
            (_) -> false
        end
    ).

%% Test 96: Non-atom element tag raises compile-time error.
%% A binary tag is accepted and taken VERBATIM, which is the point: the atom form
%% goes through the backend's `name/1`, and for `?html` that translates `_` to `-`
%% so `az_click` needs no quoting. That translation left a tag whose name really
%% contains an underscore unwritable -- `{'my-widget_v2', ...}` silently emitted
%% `<my-widget-v2>`, a different element. Same two forms, same rule, as an attribute
%% name.
binary_element_tag_is_verbatim(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_bin_tag). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {<<\"my-widget_v2\">>, [], [<<\"a\">>]}, "
        "        {'my-widget_v2', [], [<<\"b\">>]}"
        "    ]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{}))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<my-widget_v2>a</my-widget_v2>")),
    %% The atom form still translates, which is what it is for.
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<my-widget-v2>b</my-widget-v2>")).

%% A binary tag resolves through the tag-keyed backend callbacks exactly as an atom
%% does -- void detection, raw-text classification and the foreign-content switch.
binary_element_tag_classifies(Config) when is_list(Config) ->
    ?assertEqual(arizona_html:is_void(input), arizona_html:is_void(~"input")),
    ?assertEqual(arizona_html:is_void('BR'), arizona_html:is_void(~"BR")),
    ?assertEqual(
        arizona_html:raw_text_kind(script, html), arizona_html:raw_text_kind(~"script", html)
    ),
    ?assertEqual(
        arizona_html:content_context(svg, html), arizona_html:content_context(~"svg", html)
    ),
    Mod = compile_module(
        "-module(pt_bin_tag_cls). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({<<\"svg\">>, [], ["
        "        {<<\"title\">>, [], [arizona_template:get(t, Bindings)]}"
        "    ]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{t => ~"Hi"}))),
    %% Inside `<svg>` a `<title>` is parsed markup, so its slot keeps markers.
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<!--az:")).

%% A literal that is not a name at all is still a malformed element form.
non_name_literal_tag_still_invalid(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_num_tag). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({42, [], [<<\"hi\">>]}). ",
        fun(R) -> R =:= invalid_element end
    ).

%% each/2 rejects `fun name/arity` references -- a reference can't be checked to return
%% an element, so it must be inlined (or use ?stateless inside an element).
%% A local single-clause `fun Name/1` ref is resolved and inlined: when its body is a
%% valid element, the each compiles like an inline fun.
each_with_named_fun_ref(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_ref). "
        "-export([render/1, row/1]). "
        "row(Name) -> {'li', [], [Name]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(items, Bindings, []))]}). "
    ),
    ?assert(is_map(Mod:render(#{items => [~"a", ~"b"]}))).

%% A local single-clause `fun Name/2` ref over a stream/map is resolved and inlined too.
each_with_named_fun_ref_arity_2(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_ref_2). "
        "-export([render/1, row/2]). "
        "row(Item, _Key) -> {'li', [], [Item]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/2, "
        "        arizona_template:get(items, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{items => #{1 => ~"a"}}))).

%% The named-ref path runs the SAME body validation as an inline fun: a non-element body
%% (here a bare binary) is rejected with each_body_not_element.
each_named_fun_ref_non_element_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_named_non_element). "
        "-export([render/1, row/1]). "
        "row(Name) -> <<\"row:\", Name/binary>>. "
        "render(Bindings) -> "
        "    arizona_template:each(fun row/1, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% Likewise for a 2-arg named ref: a non-element body yields the stream error.
each_named_fun_ref_arity_2_non_element_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_named_non_element_2). "
        "-export([render/1, row/2]). "
        "row(Item, Key) -> <<Key/binary, \"=\", Item/binary>>. "
        "render(Bindings) -> "
        "    arizona_template:each(fun row/2, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_stream_body_not_element end
    ).

%% A conditional body is a distinct mistake from a bare value: "wrap the value in an
%% element" would have the author pick a tag, which for branches already returning
%% different elements changes the markup rather than fixing it.
%% A variable or computed tag is the OTHER route to "conditional tag" -- reached
%% before the ?each body check, and previously answered only with the accepted forms
%% and no idiom, even inside an ?each item where the body error would have taught it.
computed_element_tag_rejected_with_idiom(Config) when is_list(Config) ->
    Bare =
        "-module(pt_ct_var). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Tag = arizona_template:get(tag, Bindings), "
        "    arizona_template:html({'div', [], [{Tag, [], [~\"x\"]}]}). ",
    assert_parse_error(Bare, fun(R) -> R =:= computed_element_tag end),
    %% Computed in place, and inside an ?each item -- same answer.
    Computed =
        "-module(pt_ct_call). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each("
        "        fun(I, K) -> {maps:get(tag, I), [{az_key, K}], [~\"x\"]} end, "
        "        arizona_template:get(items, Bindings))]}). ",
    assert_parse_error(Computed, fun(R) -> R =:= computed_element_tag end).

%% The shape the old error was actually about still gets it: a tuple that is not an
%% element form at all, rather than one whose tag is merely not literal.
malformed_element_still_invalid_element(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_ct_bad). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [], [{'p', [], [~\"x\"], extra}]}). ",
        fun(R) -> R =:= invalid_element end
    ).

each_stream_control_flow_body_rejected_with_wrapper_advice(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_stream_cond). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item, Key) -> "
        "        case Item of "
        "            note -> {'em', [{az_key, Key}], [Item]}; "
        "            _ -> {'strong', [{az_key, Key}], [Item]} "
        "        end "
        "    end, arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_stream_body_control_flow end
    ).

%% The shape that error tells the author to write must actually work: one stable keyed
%% item element with the branch as a child, still diffing per item by key.
each_stream_control_flow_wrapper_fix_keys_per_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_stream_cond_fix). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(Item, Key) -> "
        "        {'li', [{az_key, Key}], [case Item of "
        "            #{kind := note, t := T} -> {'em', [], [T]}; "
        "            #{t := T} -> {'strong', [], [T]} "
        "        end]} "
        "    end, arizona_template:get(items, Bindings))]}). "
    ),
    Key = fun(I) -> maps:get(id, I) end,
    S0 = arizona_stream:new(Key, [
        #{id => 1, kind => note, t => ~"A"}, #{id => 2, kind => row, t => ~"B"}
    ]),
    T0 = Mod:render(#{items => S0}),
    %% Each branch renders under its own key, so the two items differ structurally.
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T0)),
    ?assertMatch({_, _}, binary:match(HTML, ~"az-key=\"1\"")),
    ?assertMatch({_, _}, binary:match(HTML, ~"<em")),
    ?assertMatch({_, _}, binary:match(HTML, ~"<strong")),
    {_H, Snap0} = arizona_render:render(T0),
    {_, Snap1, _} = arizona_diff:diff(T0, Snap0, #{}),
    S1 = arizona_stream:update(S0, 2, #{id => 2, kind => row, t => ~"CHANGED"}),
    {Ops, _, _} = arizona_diff:diff(Mod:render(#{items => S1}), Snap1, #{}, #{items => true}),
    %% Keyed per-item patch addressing only the changed item's inner slot.
    ?assertMatch([[?OP_ITEM_PATCH, _Container, ~"2", [[?OP_TEXT, _Inner, ~"CHANGED"]]]], Ops).

%% each/2 rejects `fun Mod:Name/Arity` remote references: the body isn't visible to inline.
each_with_remote_fun_ref(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_remote_ref). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun erlang:integer_to_binary/1, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_remote_fun_ref end
    ).

%% A named-ref callback whose element holds a conditional child diffs per-item, exactly
%% like the inline-fun equivalent (each_body_conditional_child_diffs).
each_named_fun_ref_diffs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_diffs). "
        "-export([render/1, row/1]). "
        "row(U) -> {'li', [], [case U of #{name := N} -> N; _ -> ~\"-\" end]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(users, Bindings))]}). "
    ),
    B0 = #{users => [#{name => ~"Ada"}]},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{users => [#{name => ~"Grace"}]},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A named-ref callback with a map-pattern parameter head: the pattern becomes the per-item
%% `d` fun parameter and resolves correctly.
each_named_fun_ref_pattern_param(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_pattern). "
        "-export([render/1, row/1]). "
        "row(#{name := N}) -> {'li', [], [N]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(users, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{users => [#{name => ~"Ada"}]}))).

%% A named-ref callback with a guard: the guard is carried onto the per-item `d` fun clause.
each_named_fun_ref_guard(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_guard). "
        "-export([render/1, row/1]). "
        "row(X) when is_map(X) -> {'li', [], [maps:get(n, X)]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(users, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{users => [#{n => ~"Ada"}]}))).

%% A named-ref callback with statements before the element (a prefix body): the prefix runs
%% per item inside the generated `d` fun.
each_named_fun_ref_prefix_body(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_prefix). "
        "-export([render/1, row/1]). "
        "row(I) -> N = integer_to_binary(I), {'li', [], [N]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(ns, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{ns => [1, 2]}))).

%% A named-ref callback that hoists a variable whose name the CALLER also bound.
%% Inlining spliced the callee's binder into the caller's scope, where it stopped
%% being a binding and became an equality test against the caller's value: the item
%% rendered for as long as the two agreed and raised `badmatch` the first time they
%% differed. The callee's variables are alpha-renamed, so the scopes stay separate
%% and each side keeps its own `side`.
each_named_fun_ref_shadows_caller_var(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_shadow). "
        "-export([render/1, row/1]). "
        "row(Bindings) -> Side = arizona_template:get(side, Bindings), {'li', [], [Side]}. "
        "render(Bindings) -> "
        "    Side = arizona_template:get(side, Bindings), "
        "    arizona_template:html({'ul', [{class, Side}], [arizona_template:each(fun row/1, "
        "        arizona_template:get(rows, Bindings))]}). "
    ),
    T = Mod:render(#{side => ~"buy", rows => [#{side => ~"sell"}]}),
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    %% The caller's `side` reaches the class, the item's reaches the row.
    ?assertMatch({_, _}, binary:match(HTMLBin, ~"class=\"buy\"")),
    ?assertMatch({_, _}, binary:match(HTMLBin, ~"sell")).

%% A named-ref callback whose body itself contains a nested ?each (with its own named ref):
%% both resolve via the module-global lookup and the recursive compile path.
each_named_fun_ref_loc_names_callee(Config) when is_list(Config) ->
    %% The callee is inlined, so it is not a compiled function any more and the
    %% crash carries no frame of its own. The `arizona_loc` wrapper is therefore
    %% the only lexical hint, and surfaces that catch without a stacktrace (the
    %% dev MCP's render_component) show nothing else -- so it must name the
    %% callee's clause, not the `?each` call site in the caller.
    Mod = compile_module(
        "-module(pt_each_named_loc).\n"
        "-export([render/1]).\n"
        "row(Bindings) ->\n"
        "    Side = arizona_template:get(side, Bindings),\n"
        "    {'li', [], [erlang:error({boom, Side})]}.\n"
        "render(Bindings) ->\n"
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1,\n"
        "        arizona_template:get(rows, Bindings))]}).\n"
    ),
    T = Mod:render(#{rows => [#{side => ~"sell"}]}),
    %% The crashing slot is row/1's line 5; the ?each call site is line 7.
    ?assertError(
        {arizona_loc, {pt_each_named_loc, 5}, {boom, ~"sell"}},
        arizona_render:render_to_iolist(T)
    ),
    %% A crash before the element runs at the ?each level, which is stamped with
    %% row/1's clause head (line 3), still not the call site.
    Mod2 = compile_module(
        "-module(pt_each_named_loc2).\n"
        "-export([render/1]).\n"
        "row(Bindings) ->\n"
        "    {ok, Side} = erlang:error({boom, arizona_template:get(side, Bindings)}),\n"
        "    {'li', [], [Side]}.\n"
        "render(Bindings) ->\n"
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1,\n"
        "        arizona_template:get(rows, Bindings))]}).\n"
    ),
    ?assertError(
        {arizona_loc, {pt_each_named_loc2, 3}, {boom, ~"sell"}},
        arizona_render:render_to_iolist(Mod2:render(#{rows => [#{side => ~"sell"}]}))
    ).

each_named_fun_ref_nested_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_nested). "
        "-export([render/1, section/1, item/1]). "
        "item(I) -> {'li', [], [I]}. "
        "section(S) -> {'ul', [], [arizona_template:each(fun item/1, "
        "    arizona_template:get(items, S))]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [arizona_template:each(fun section/1, "
        "        arizona_template:get(sections, Bindings))]}). "
    ),
    T = Mod:render(#{sections => [#{items => [~"a", ~"b"]}]}),
    ?assert(is_map(T)),
    %% The nested ?each inside the resolved section/1 clause must be transformed --
    %% left as a raw runtime stub it crashes (function_clause) when the per-item
    %% template is rendered, so render it out and assert both levels appear.
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<ul")),
    ?assertEqual(2, length(binary:matches(HTMLBin, ~"<li"))).

%% P5 companion: a nested ?each AND a nested ?html inside an INLINE named-fun each
%% callback body -- both are template macros that must be transformed in the resolved
%% clause. Renders correctly only when the resolved body is re-walked by the transform.
each_named_fun_ref_nested_macros_render(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_nested_macros). "
        "-export([render/1, row/1]). "
        "row(R) -> {'div', [], ["
        "    arizona_template:html({'span', [], [arizona_template:get(title, R)]}),"
        "    {'ul', [], [arizona_template:each(fun(I) -> {'li', [], [I]} end, "
        "        arizona_template:get(items, R))]}"
        "]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'main', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(rows, Bindings))]}). "
    ),
    T = Mod:render(#{rows => [#{title => ~"T", items => [~"a", ~"b"]}]}),
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<span")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"T")),
    ?assertEqual(2, length(binary:matches(HTMLBin, ~"<li"))).

%% A multi-clause local ref can't map to one shared per-item template -- rejected.
each_named_fun_multi_clause_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_named_multi). "
        "-export([render/1, row/1]). "
        "row(a) -> {'li', [], [~\"a\"]}; "
        "row(X) -> {'li', [], [X]}. "
        "render(Bindings) -> "
        "    arizona_template:each(fun row/1, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_named_fun_multi_clause end
    ).

%% A local ref of an arity other than 1 or 2 isn't a valid callback -- rejected as
%% invalid_each_fun (1 = list item, 2 = stream/map item+key).
each_named_fun_wrong_arity_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_named_arity3). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun row/3, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= invalid_each_fun end
    ).

%% A local ref to a function not defined in this module -- rejected with a clear message.
each_named_fun_undefined_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_named_undef). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun missing_row/1, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_named_fun_undefined end
    ).

%% Load-bearing: a PRIVATE (non-exported) callback used only by ?each is inlined, orphaning
%% it. The injected nowarn_unused_function attribute must let this compile under
%% warnings_as_errors (compile_module_strict). Without the injection, the unused `row/1`
%% would fail the build.
each_named_fun_ref_private_local_strict(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_each_named_private). "
        "-export([render/1]). "
        "row(N) -> {'li', [], [N]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(items, Bindings, []))]}). "
    ),
    ?assert(is_map(Mod:render(#{items => [~"a", ~"b"]}))).

%% A same-module explicit ref `fun ?MODULE:row/1` (here the literal module name) resolves to
%% the local body exactly like `fun row/1`. With a PRIVATE callback, compile_module_strict
%% also proves the suppression covers the same-module form under warnings_as_errors.
each_named_fun_same_module_ref(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_each_same_mod). "
        "-export([render/1]). "
        "row(N) -> {'li', [], [N]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun pt_each_same_mod:row/1, "
        "        arizona_template:get(items, Bindings, []))]}). "
    ),
    ?assert(is_map(Mod:render(#{items => [~"a", ~"b"]}))).

%% An imported function referenced as a bare `fun row/1` looks local but its body lives in
%% another module, so it isn't in this module's FunDefs -- rejected as undefined.
each_named_fun_imported_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_imported). "
        "-export([render/1]). "
        "-import(other_mod, [row/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun row/1, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_named_fun_undefined end
    ).

%% A remote ref whose module is a variable (`fun M:row/1`) can't be resolved -- rejected.
each_named_fun_variable_module_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_var_mod). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    M = other_mod, "
        "    arizona_template:each(fun M:row/1, "
        "        arizona_template:get(items, Bindings, [])). ",
        fun(R) -> R =:= each_remote_fun_ref end
    ).

%% A named-fun ?each inside ?native is marked for the native backend: the callback is
%% resolved/inlined with the native backend, and a PRIVATE callback is suppressed the same
%% way (compile_module_strict -> warnings_as_errors).
each_named_fun_ref_native(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_each_named_native). "
        "-export([render/1]). "
        "row(#{text := T}, Key) -> {'Text', [{az_key, Key}], [T]}. "
        "render(Bindings) -> "
        "    arizona_template:native({'Column', [{id, arizona_template:get(id, Bindings)}], "
        "        [arizona_template:each(fun row/2, arizona_template:get(items, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{id => ~"c", items => #{1 => #{text => ~"a"}}}))).

%% A named-ref callback whose body is ?html(...) is unwrapped and diffs per-item, exactly
%% like the bare-element named-ref form (each_named_fun_ref_diffs).
each_named_fun_ref_html_body_diffs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_named_html). "
        "-export([render/1, row/1]). "
        "row(U) -> arizona_template:html({'li', [], "
        "    [case U of #{name := N} -> N; _ -> ~\"-\" end]}). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun row/1, "
        "        arizona_template:get(users, Bindings))]}). "
    ),
    B0 = #{users => [#{name => ~"Ada"}]},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{users => [#{name => ~"Grace"}]},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A named-ref callback whose body is ?native(...) inside a ?native(...) view unwraps too
%% (the private row/2 is covered by the injected nowarn_unused_function / ignore_xref).
each_named_fun_ref_native_html(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_each_named_native_html). "
        "-export([render/1]). "
        "row(#{text := T}, Key) -> arizona_template:native({'Text', [{az_key, Key}], [T]}). "
        "render(Bindings) -> "
        "    arizona_template:native({'Column', [{id, arizona_template:get(id, Bindings)}], "
        "        [arizona_template:each(fun row/2, arizona_template:get(items, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{id => ~"c", items => #{1 => #{text => ~"a"}}}))).

%% A whole-body ?html(...) callback is unwrapped to its element and builds the SAME per-item
%% template as returning the element bare -- it compiles, renders, and diffs per item (no
%% crash on diff). The diff ops are byte-identical to the bare-element form.
each_body_html_wrapped_diffs(Config) when is_list(Config) ->
    Wrapped = compile_module(
        "-module(pt_each_html_wrapped). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(U) -> "
        "        arizona_template:html({'li', [], [case U of #{name := N} -> N; _ -> ~\"-\" end]}) "
        "    end, arizona_template:get(users, Bindings))]}). "
    ),
    Bare = compile_module(
        "-module(pt_each_html_bare). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(U) -> "
        "        {'li', [], [case U of #{name := N} -> N; _ -> ~\"-\" end]} "
        "    end, arizona_template:get(users, Bindings))]}). "
    ),
    B0 = #{users => [#{name => ~"Ada"}]},
    B1 = #{users => [#{name => ~"Grace"}]},
    Changed = compute_changed(B0, B1),
    {_HW, WSnap0, WV0} = arizona_render:render(Wrapped:render(B0), #{}),
    {WOps, _, _} = arizona_diff:diff(Wrapped:render(B1), WSnap0, WV0, Changed),
    {_HB, BSnap0, BV0} = arizona_render:render(Bare:render(B0), #{}),
    {BOps, _, _} = arizona_diff:diff(Bare:render(B1), BSnap0, BV0, Changed),
    ?assertNotEqual([], WOps),
    ?assertEqual(BOps, WOps).

%% A bare ?stateful/?stateless descriptor body is rejected -- wrap it in an element.
each_body_descriptor_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_descriptor). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(X) -> "
        "        arizona_template:stateful(some_component, #{id => X}) "
        "    end, arizona_template:get(xs, Bindings))]}). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% A case (or any control-flow) body is rejected: put the conditional inside an element.
each_body_case_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_case). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(U) -> "
        "        case U of "
        "            #{name := N} -> arizona_template:html({'li', [], [N]}); "
        "            _ -> ~\"-\" "
        "        end "
        "    end, arizona_template:get(users, Bindings))]}). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% An `if` (like any branching control flow) is rejected: branches may select different
%% per-item structures, which is exactly the fragile case -- put it inside an element.
each_body_if_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_if). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(X) -> "
        "        if is_binary(X) -> {'li', [], [X]}; true -> {'span', [], [X]} end "
        "    end, arizona_template:get(xs, Bindings))]}). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% A runtime binary construction is a plain value -- rejected; use a comprehension.
each_body_runtime_binary_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_runtime_bin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(X) -> "
        "        <<\"row \", X/binary>> "
        "    end, arizona_template:get(xs, Bindings, [])). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% The 2-arg (stream/map) callback is validated like the 1-arg one, but raises the
%% stream-specific error (the fix advice differs: no comprehension fallback for a stream).
each_body_two_arg_value_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_two_arg). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(Item, _Key) -> "
        "        Item "
        "    end, arizona_template:get(xs, Bindings))]}). ",
        fun(R) -> R =:= each_stream_body_not_element end
    ).

%% A 2-arg (stream/map) callback whose whole body is ?html(...) is unwrapped the same way
%% as the 1-arg form, building a per-item template that renders.
each_body_two_arg_html_diffs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_two_arg_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(V, K) -> "
        "        arizona_template:html({'li', [], [K, ~\": \", V]}) "
        "    end, arizona_template:get(items, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{items => #{~"a" => ~"1"}}))).

%% A ?html template as a *list item* ([?html(...)]) is still rejected -- only a WHOLE-body
%% wrapper is unwrapped; a one-template list lands in the fragile per-item value slot.
each_body_list_with_html_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_list_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(X) -> "
        "        [arizona_template:html({'li', [], [X]})] "
        "    end, arizona_template:get(xs, Bindings))]}). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% A bare list whose item is a descriptor (or template) is rejected: the item lands in a
%% per-item value slot and crashes on diff. Wrap it in an element instead of a bare list.
each_body_list_with_descriptor_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_each_list_desc). "
        "-export([render/1, row/1]). "
        "row(P) -> arizona_template:html({'span', [], [arizona_template:get(t, P)]}). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(X) -> "
        "        [arizona_template:stateless(fun row/1, #{t => X})] "
        "    end, arizona_template:get(xs, Bindings))]}). ",
        fun(R) -> R =:= each_body_not_element end
    ).

%% A mixed static/dynamic list fragment compiles to a per-item template (accepted).
each_body_mixed_fragment_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(X) -> "
        "        [~\"* \", X] "
        "    end, arizona_template:get(xs, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{xs => [~"a", ~"b"]}))).

%% The diff-safe rewrite of a conditional item: the case is a CHILD of an element literal,
%% so the item stays a stable per-item template and diffs cleanly (the rejected case-body
%% above would have crashed on this same diff).
each_body_conditional_child_diffs(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_cond_child). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(U) -> "
        "        {'li', [], [case U of #{name := N} -> N; _ -> ~\"-\" end]} "
        "    end, arizona_template:get(users, Bindings))]}). "
    ),
    B0 = #{users => [#{name => ~"Ada"}]},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{users => [#{name => ~"Grace"}]},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% The parse transform rewrites `arizona_template:stateless(atom, Props)` into
%% `arizona_template:stateless(fun atom/1, Props)` so bare atoms and fun
%% references are interchangeable.
stateless_with_atom_callback(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_stateless_atom). "
        "-export([render/1, card/1]). "
        "card(#{label := L}) -> L. "
        "render(Bindings) -> "
        "    arizona_template:stateless(card, #{label => maps:get(label, Bindings)}). "
    ),
    #{callback := CB, props := Props} = Mod:render(#{label => <<"ok">>}),
    ?assert(is_function(CB, 1)),
    ?assertEqual(<<"ok">>, CB(Props)).

stateless_with_fun_ref_callback(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_stateless_funref). "
        "-export([render/1, card/1]). "
        "card(#{label := L}) -> L. "
        "render(Bindings) -> "
        "    arizona_template:stateless(fun card/1, #{label => maps:get(label, Bindings)}). "
    ),
    #{callback := CB, props := Props} = Mod:render(#{label => <<"ok">>}),
    ?assert(is_function(CB, 1)),
    ?assertEqual(<<"ok">>, CB(Props)).

%% Remote fun refs (`fun Mod:Fn/1`) pass through the parse transform unchanged
%% and work via the runtime `is_function(Callback, 1)` guard.
stateless_with_remote_fun_ref_callback(Config) when is_list(Config) ->
    %% Helper module with the callback.
    _HelperMod = compile_module(
        "-module(pt_stateless_helper). "
        "-export([card/1]). "
        "card(#{label := L}) -> L. "
    ),
    Mod = compile_module(
        "-module(pt_stateless_remote_funref). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:stateless(fun pt_stateless_helper:card/1, "
        "        #{label => maps:get(label, Bindings)}). "
    ),
    #{callback := CB, props := Props} = Mod:render(#{label => <<"hi">>}),
    ?assert(is_function(CB, 1)),
    ?assertEqual(<<"hi">>, CB(Props)).

%% Test 97: each/2 with 2-arity fun raises compile-time error.
invalid_each_fun_arity(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_each_arity). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each("
        "        fun(A, B, C) -> {'div', [], [A]} end,"
        "        arizona_template:get(items, Bindings, [])"
        "    ). ",
        fun
            (invalid_each_fun) -> true;
            (_) -> false
        end
    ).

cross_target_html_in_native(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_html_in_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native({'Column', [], ["
        "        arizona_template:html({'div', [], [arizona_template:get(x, Bindings)]})"
        "    ]}). ",
        fun
            (cross_target_nesting) -> true;
            (_) -> false
        end
    ).

cross_target_native_in_html(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_native_in_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        arizona_template:native({'Text', [], [arizona_template:get(x, Bindings)]})"
        "    ]}). ",
        fun
            (cross_target_nesting) -> true;
            (_) -> false
        end
    ).

cross_target_siblings_compile(Config) when is_list(Config) ->
    %% A dual-serve render with ?html and ?native in sibling clauses (not nested
    %% in one another) must compile -- the guard rejects only inline nesting.
    Mod = compile_module(
        "-module(pt_dual_serve). "
        "-export([render/1]). "
        "render(#{target := html} = Bindings) -> "
        "    arizona_template:html({'div', [], [arizona_template:get(x, Bindings)]}); "
        "render(#{target := native} = Bindings) -> "
        "    arizona_template:native({'Text', [], [arizona_template:get(x, Bindings)]}). "
    ),
    ?assertEqual(pt_dual_serve, Mod).

%% Test 98: each/2 with non-fun raises compile-time error.
invalid_each_not_fun(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_each_notfun). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each("
        "        my_fun,"
        "        arizona_template:get(items, Bindings, [])"
        "    ). ",
        fun
            (invalid_each_fun) -> true;
            (_) -> false
        end
    ).

%% Test 99: Invalid attribute name (integer) falls to invalid_attribute error.
invalid_attribute_name(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_attr_name). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{123, <<\"val\">>}], [<<\"text\">>]}"
        "    ). ",
        fun
            (invalid_attribute) -> true;
            (_) -> false
        end
    ).

%% Test 100: each/2 with multi-clause fun raises compile-time error.
invalid_each_multi_clause(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_each_multi). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each("
        "        fun(a) -> {'div', [], [<<\"a\">>]};"
        "           (b) -> {'div', [], [<<\"b\">>]} end,"
        "        arizona_template:get(items, Bindings, [])"
        "    ). ",
        fun
            (invalid_each_fun) -> true;
            (_) -> false
        end
    ).

%% Test 101: Invalid attribute name with boolean true value.
invalid_attribute_name_true(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_attr_name_true). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{123, true}], [<<\"text\">>]}"
        "    ). ",
        fun
            (invalid_attribute) -> true;
            (_) -> false
        end
    ).

%% Test 102: Invalid attribute name with boolean false value.
invalid_attribute_name_false(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_attr_name_false). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{123, false}], [<<\"text\">>]}"
        "    ). ",
        fun
            (invalid_attribute) -> true;
            (_) -> false
        end
    ).

%% Test 103: Wrong arity element tuple (1-element).
invalid_element_single(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_elem_single). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div'}). ",
        fun
            (invalid_element) -> true;
            (_) -> false
        end
    ).

%% Test 104: Wrong arity element tuple (4-element).
invalid_element_four(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_elem_four). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [], extra}). ",
        fun
            (invalid_element) -> true;
            (_) -> false
        end
    ).

%% Test 105: Invalid static child -- empty tuple literal.
invalid_child_empty_tuple(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_child_tuple). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [{}]}). ",
        fun
            ({invalid_child, _}) -> true;
            (_) -> false
        end
    ).

%% Test 106: Invalid static child -- non-empty tuple literal.
invalid_child_tuple_literal(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_child_tuple2). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [{1, 2}]}). ",
        fun
            ({invalid_child, _}) -> true;
            (_) -> false
        end
    ).

%% Test 107: Invalid static child -- tuple among valid children.
invalid_child_mixed(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_child_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [<<\"ok\">>, {}]}). ",
        fun
            ({invalid_child, _}) -> true;
            (_) -> false
        end
    ).

%% ==========================================================================
%% az: module parse transform equivalence tests
%% ==========================================================================

%% Test 108: az:html static -- same output as static_only_test.
az_html_static(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_static). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html({'p', [{class, <<\"greeting\">>}], [<<\"Hello!\">>]}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<p class=\"greeting\">Hello!</p>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)),
    ?assert(is_binary(maps:get(f, T))).

%% Test 109: az:html dynamic text -- same structure as single_dynamic_text_test.
az_html_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_dyn_text). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html("
        "        {'p', [], [<<\"Hello, \">>, az:get(name, Bindings, <<\"world\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{name => <<"Alice">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<p az=\"0\">Hello, <!--az:0-->">>,
                <<"<!--/az--></p>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_az_dyn_text, 1}}] = maps:get(d, T),
    ?assertEqual(<<"Alice">>, Fun()).

%% Test 110: az:html dynamic attr -- same structure as single_dynamic_attr_test.
az_html_dynamic_attr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_dyn_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html("
        "        {'div', [{class, az:get(theme, Bindings, <<\"light\">>)}], [<<\"content\">>]}"
        "    ). "
    ),
    T = Mod:render(#{theme => <<"dark">>}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<">content</div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {attr, <<"class">>, Fun}, {pt_az_dyn_attr, 1}}] = maps:get(d, T),
    ?assertEqual(<<"dark">>, Fun()).

%% Test 111: az:each basic -- produces each descriptor.
az_each_basic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_each_basic). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:each(fun(Item) -> "
        "        {'li', [], [maps:get(name, Item)]} "
        "    end, az:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [#{name => <<"Alice">>}]}),
    ?assertEqual(0, maps:get(t, Result)),
    Tmpl = maps:get(template, Result),
    ?assertEqual(0, maps:get(t, Tmpl)),
    Fp = maps:get(f, Tmpl),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<li az=\"0\"><!--az:0-->">>,
                <<"<!--/az--></li>">>
            ]
        ),
        maps:get(s, Tmpl)
    ),
    DFun = maps:get(d, Tmpl),
    ?assert(is_function(DFun, 1)),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_az_each_basic, 1}}] = DFun(#{name => <<"Alice">>}),
    ?assertEqual(<<"Alice">>, Fun()).

%% A single-root list item (one top-level element) is stamped `single_root => true`
%% on its item template, so the diff can patch items positionally (OP_LIST_PATCH).
each_single_root_flag(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_single_root). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:each(fun(Item) -> "
        "        {'li', [], [maps:get(name, Item)]} "
        "    end, az:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [#{name => <<"Alice">>}]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual(true, maps:get(single_root, Tmpl)).

%% A multi-root list item (a fragment / list of elements) has no unambiguous
%% per-position DOM node, so it is NOT flagged single_root (falls back to the
%% wholesale re-render).
each_multi_root_no_single_root_flag(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_multi_root). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:each(fun(Item) -> "
        "        [{'dt', [], [maps:get(k, Item)]}, {'dd', [], [maps:get(v, Item)]}] "
        "    end, az:get(items, Bindings, [])). "
    ),
    Result = Mod:render(#{items => [#{k => <<"a">>, v => <<"1">>}]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual(undefined, maps:get(single_root, Tmpl, undefined)).

%% A stream (2-arg callback) item is keyed by `az-key`, not by position, so it is
%% NOT flagged single_root (the positional path is plain-list only).
each_stream_no_single_root_flag(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_stream). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:each(fun(Item, Key) -> "
        "        {'li', [{'az-key', Key}], [maps:get(name, Item)]} "
        "    end, az:get(stream, Bindings)). "
    ),
    Stream = arizona_stream:new(fun(I) -> maps:get(id, I) end, [#{id => 1, name => <<"A">>}]),
    Result = Mod:render(#{stream => Stream}),
    Tmpl = maps:get(template, Result),
    %% `single_root` describes the item BODY (one top-level element), not the
    %% source kind -- and a 2-arg callback compiles to one template that serves
    %% both a stream and a map. A map source needs the flag to patch positionally
    %% (`arizona_diff:diff_map/4`); a stream ignores it outright, since it keys
    %% items by `az-key` and never reaches the positional walk.
    ?assertEqual(true, maps:get(single_root, Tmpl, undefined)).

%% A `?native` single-root list each is NOT flagged single_root: the gate is the
%% backend's `supports_list_patch/0` callback (asked at compile time), and
%% arizona_native returns false because no native client implements `?OP_LIST_PATCH`
%% -- so a native list each keeps the wholesale path. Contrast each_single_root_flag
%% (html: supports_list_patch/0 is true -> single_root).
each_native_no_single_root_flag(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_native_gate). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:each(fun(Item) -> "
        "        {'Text', [], [maps:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings), arizona_native, html). "
    ),
    Result = Mod:render(#{items => [#{name => <<"a">>}]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual(undefined, maps:get(single_root, Tmpl, undefined)).

%% Test 112: az:each nested inside az:html.
az_each_inside_html(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_each_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html("
        "        {'div', [{id, az:get(id, Bindings, <<\"list\">>)}], ["
        "            az:each(fun(Item) -> "
        "                {'li', [], [maps:get(text, Item)]} "
        "            end, az:get(items, Bindings, []))"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{id => <<"mylist">>, items => [#{text => <<"hi">>}]}),
    Fp = maps:get(f, T),
    ?assertEqual(
        scope_s(
            Fp,
            [
                <<"<div az=\"0\"">>,
                <<"><!--az:0-->">>,
                <<"<!--/az--></div>">>
            ]
        ),
        maps:get(s, T)
    ),
    Az0 = <<Fp/binary, "-0">>,
    [
        {Az0, {attr, <<"id">>, IdFun}, {pt_az_each_nested, 1}},
        {Az0, EachFun, {pt_az_each_nested, 1}}
    ] = maps:get(d, T),
    ?assertEqual(<<"mylist">>, IdFun()),
    EachResult = EachFun(),
    ?assertEqual(0, maps:get(t, EachResult)),
    Tmpl = maps:get(template, EachResult),
    DFun = maps:get(d, Tmpl),
    InnerFp = maps:get(f, Tmpl),
    InnerAz0 = <<InnerFp/binary, "-0">>,
    [{InnerAz0, {esc, ItemFun}, {pt_az_each_nested, 1}}] = DFun(#{text => <<"hi">>}),
    ?assertEqual(<<"hi">>, ItemFun()).

%% Test 113: az:html single element with az-nodiff -- no markers, no az attrs.
az_layout(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_layout). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html("
        "        {'p', ['az-nodiff'], [maps:get(text, Bindings, <<\"hi\">>)]}"
        "    ). "
    ),
    T = Mod:render(#{text => <<"hello">>}),
    ?assertEqual([<<"<p">>, <<">">>, <<"</p>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [
        {undefined, {attr, <<"az-nodiff">>, _}, _},
        {undefined, {esc, Fun}, {pt_az_layout, 1}}
    ] = maps:get(d, T),
    ?assert(is_function(Fun, 0)),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 114: az:html mixed list with az-nodiff -- binary + element.
az_layout_mixed(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_layout_mixed). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html(["
        "        <<\"<!DOCTYPE html>\">>, "
        "        {'html', ['az-nodiff'], ["
        "            {'head', [], [{'title', [], [maps:get(title, Bindings, <<\"Test\">>)]}]}"
        "        ]}"
        "    ]). "
    ),
    T = Mod:render(#{title => <<"Hello">>}),
    Statics = maps:get(s, T),
    ?assertEqual([<<"<!DOCTYPE html><html><head><title>">>, <<"</title></head></html>">>], Statics),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {esc, Fun}, {pt_az_layout_mixed, 1}}] = maps:get(d, T),
    ?assertEqual(<<"Hello">>, Fun()).

%% Test 115: az:html nested elements -- az counter threading at depth.
az_html_nested(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    az:html("
        "        {'div', [], ["
        "            {'span', [], [az:get(a, Bindings, <<\"x\">>)]},"
        "            {'span', [], [az:get(b, Bindings, <<\"y\">>)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{a => <<"hello">>, b => <<"world">>}),
    Fp = maps:get(f, T),
    ?assert(is_binary(Fp)),
    %% Should have two dynamics for the two get calls
    Dynamics = maps:get(d, T),
    ?assertEqual(2, length(Dynamics)),
    [{_, {esc, FunA}, {pt_az_nested, 1}}, {_, {esc, FunB}, {pt_az_nested, 1}}] = Dynamics,
    ?assertEqual(<<"hello">>, FunA()),
    ?assertEqual(<<"world">>, FunB()).

%% ============================================================================
%% az_view group -- auto-injection and validation
%% ============================================================================

az_view_auto_injected(Config) when is_list(Config) ->
    %% Root element of arizona_stateful render/1 gets az-view auto-injected
    Mod = compile_module(
        "-module(pt_az_view_inject). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"hi\">>]}). "
    ),
    T = Mod:render(#{id => <<"v">>}),
    Statics = maps:get(s, T),
    StaticsBin = iolist_to_binary(Statics),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_explicit_on_root_ok(Config) when is_list(Config) ->
    %% User can explicitly set az_view on the root -- no error
    Mod = compile_module(
        "-module(pt_az_view_explicit). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [{id, az:get(id, Bindings)}, az_view], [<<\"hi\">>]}). "
    ),
    T = Mod:render(#{id => <<"v">>}),
    Statics = maps:get(s, T),
    StaticsBin = iolist_to_binary(Statics),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_on_child_raises(Config) when is_list(Config) ->
    %% az_view on a non-root child element raises compile error
    assert_parse_error(
        "-module(pt_az_view_child). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, az:get(id, Bindings)}], ["
        "            {'span', [az_view], [<<\"bad\">>]}"
        "        ]}"
        "    ). ",
        fun
            (az_view_not_allowed) -> true;
            (_) -> false
        end
    ).

az_view_in_stateless_raises(Config) when is_list(Config) ->
    %% az_view in a non-arizona_stateful module raises compile error
    assert_parse_error(
        "-module(pt_az_view_stateless). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [az_view], [<<\"bad\">>]}). ",
        fun
            (az_view_not_allowed) -> true;
            (_) -> false
        end
    ).

az_view_explicit_tuple_true(Config) when is_list(Config) ->
    %% {az_view, true} tuple form is recognized and allowed on root
    Mod = compile_module(
        "-module(pt_az_view_tuple). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, az:get(id, Bindings)}, {az_view, true}], [<<\"hi\">>]}). "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_binary_form_raises(Config) when is_list(Config) ->
    %% <<"az-view">> binary form raises in non-live context
    assert_parse_error(
        "-module(pt_az_view_bin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [<<\"az-view\">>], [<<\"bad\">>]}). ",
        fun
            (az_view_not_allowed) -> true;
            (_) -> false
        end
    ).

%% `az` is the element's diff address: the transform emits it on every element
%% carrying dynamics, and `scope_static/2` fingerprint-scopes every ` az="` it
%% finds -- including a template-authored one, which can then equal a real slot
%% address. The client resolves a slot with querySelector, taking the first match
%% in document order, so the op patches the wrong element. Reject every literal
%% form (`{az, V}`, bare `az`, `<<"az">>`) at compile time.
reserved_az_attr_raises(Config) when is_list(Config) ->
    Reserved = fun
        ({reserved_attr, ~"az"}) -> true;
        (_) -> false
    end,
    assert_parse_error(
        "-module(pt_reserved_az_tuple). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{az, <<\"0\">>}], [arizona_template:get(x, Bindings)]}"
        "    ). ",
        Reserved
    ),
    assert_parse_error(
        "-module(pt_reserved_az_atom). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [az], [arizona_template:get(x, Bindings)]}"
        "    ). ",
        Reserved
    ),
    assert_parse_error(
        "-module(pt_reserved_az_bin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [<<\"az\">>], [arizona_template:get(x, Bindings)]}"
        "    ). ",
        Reserved
    ),
    %% A static element gets no injected `az`, so the authored one would be the
    %% only holder of a scoped address -- the collision case. Still rejected.
    assert_parse_error(
        "-module(pt_reserved_az_static). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [{az, <<\"0\">>}], [<<\"static\">>]}). ",
        Reserved
    ).

%% `az-local` is the ?local descriptor the transform injects; an authored one is
%% emitted beside it (a duplicate the client's scan reads at most one of) or,
%% with no ?local on the element, describes slots that do not exist.
reserved_az_local_attr_raises(Config) when is_list(Config) ->
    Reserved = fun
        ({reserved_attr, ~"az-local"}) -> true;
        (_) -> false
    end,
    %% The underscore atom form normalizes to the same dashed name the HTML
    %% backend emits, so both spellings are caught.
    assert_parse_error(
        "-module(pt_reserved_local_underscore). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{az_local, <<\"{}\">>}], "
        "                [arizona_template:local(<<\"k\">>, <<\"i\">>)]}"
        "    ). ",
        Reserved
    ),
    assert_parse_error(
        "-module(pt_reserved_local_dashed). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [{'az-local', <<\"{}\">>}], [<<\"x\">>]}). ",
        Reserved
    ).

%% The rejection must stay narrow: every other `az-*` name belongs to the
%% template author. `az_key` keys stream items, the event attributes carry
%% effects, `az_view` has its own (permissive on a live root) check, and an app
%% may invent its own `az-*` names.
az_prefixed_user_attrs_allowed(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_user_attrs). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'ul', [{'az-drop', <<\"reorder\">>}], ["
        "            {'li', [{az_key, <<\"k1\">>}, "
        "                    {az_click, arizona_js:push_event(<<\"pick\">>)}, "
        "                    {'az-mine', <<\"1\">>}, "
        "                    az_navigate], "
        "                   [arizona_template:get(x, Bindings)]}"
        "        ]}"
        "    ). "
    ),
    StaticsBin = iolist_to_binary(maps:get(s, Mod:render(#{x => ~"v"}))),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, ~"az-key=\"k1\"")),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, ~"az-drop=\"reorder\"")),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, ~"az-mine=\"1\"")),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, ~"az-click=")),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, ~"az-navigate")),
    %% An `az_view` on a live root is still accepted by its own check.
    LiveMod = compile_module(
        "-module(pt_az_user_attrs_live). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {#{id => maps:get(id, B, <<\"v\">>)}, #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, az:get(id, Bindings)}, az_view], [<<\"hi\">>]}"
        "    ). "
    ),
    LiveStatics = iolist_to_binary(maps:get(s, LiveMod:render(#{id => ~"v"}))),
    ?assertNotEqual(nomatch, binary:match(LiveStatics, ~"az-view")).

az_view_case_in_render(Config) when is_list(Config) ->
    %% case expression in render/1 -- az-view auto-injected in each branch
    Mod = compile_module(
        "-module(pt_az_view_case). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    case maps:get(mode, Bindings, a) of "
        "        a -> arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"a\">>]}); "
        "        b -> arizona_template:html({'span', [{id, az:get(id, Bindings)}], [<<\"b\">>]}) "
        "    end. "
    ),
    T = Mod:render(#{id => <<"v">>, mode => a}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_if_in_render(Config) when is_list(Config) ->
    %% if expression in render/1 -- az-view auto-injected
    Mod = compile_module(
        "-module(pt_az_view_if). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    if true -> "
        "        arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"ok\">>]}) "
        "    end. "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_try_in_render(Config) when is_list(Config) ->
    %% try expression in render/1 -- az-view auto-injected in body and catch
    Mod = compile_module(
        "-module(pt_az_view_try). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    try "
        "        arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"ok\">>]}) "
        "    catch _:_ -> "
        "        arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"err\">>]}) "
        "    end. "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_receive_in_render(Config) when is_list(Config) ->
    %% receive with after in render/1 -- az-view auto-injected
    Mod = compile_module(
        "-module(pt_az_view_recv). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    receive "
        "        {html, T} -> T "
        "    after 0 -> "
        "        arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"timeout\">>]}) "
        "    end. "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_block_in_render(Config) when is_list(Config) ->
    %% begin...end block in render/1 -- az-view auto-injected on last expr
    Mod = compile_module(
        "-module(pt_az_view_block). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    begin "
        "        _X = ok, "
        "        arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"block\">>]}) "
        "    end. "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_maybe_in_render(Config) when is_list(Config) ->
    %% maybe...end in render/1 -- az-view auto-injected on last expr
    Mod = compile_module(
        "-module(pt_az_view_maybe). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    maybe "
        "        arizona_template:html({'div', [{id, az:get(id, Bindings)}], [<<\"maybe\">>]}) "
        "    end. "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)).

az_view_multi_clause_render(Config) when is_list(Config) ->
    %% Multiple clauses in render/1 -- each clause gets az-view
    Mod = compile_module(
        "-module(pt_az_view_multi). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(#{mode := a} = B) -> "
        "    arizona_template:html({'div', [{id, arizona_template:get(id, B)}], [<<\"a\">>]}); "
        "render(B) -> "
        "    arizona_template:html({'span', [{id, arizona_template:get(id, B)}], [<<\"b\">>]}). "
    ),
    T1 = Mod:render(#{id => <<"v">>, mode => a}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(maps:get(s, T1)), <<"az-view">>)),
    T2 = Mod:render(#{id => <<"v">>}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(maps:get(s, T2)), <<"az-view">>)).

az_view_no_html_in_render(Config) when is_list(Config) ->
    %% render/1 that doesn't use ?html -- no injection, no error
    Mod = compile_module(
        "-module(pt_az_view_nohtml). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    maps:get(template, Bindings). "
    ),
    %% Just verify it compiles -- render returns whatever template is
    ?assert(is_atom(Mod)).

az_view_static_id_raises(Config) when is_list(Config) ->
    %% Static {id, <<"v">>} on root of arizona_stateful raises compile error
    assert_parse_error(
        "-module(pt_az_view_static_id). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [{id, <<\"v\">>}], [<<\"bad\">>]}). ",
        fun
            (live_render_id_must_be_get_id) -> true;
            (_) -> false
        end
    ).

az_view_composed_id_raises(Config) when is_list(Config) ->
    %% Composed {id, [<<"a">>, <<"b">>]} on root raises compile error
    assert_parse_error(
        "-module(pt_az_view_composed_id). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {maps:merge(#{id => <<\"v\">>}, B), #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [{id, [<<\"a\">>, <<\"b\">>]}], [<<\"bad\">>]}). ",
        fun
            (live_render_id_must_be_get_id) -> true;
            (_) -> false
        end
    ).

%% ============================================================================
%% Terminal (ANSI) render target
%% ============================================================================

cross_target_html_in_terminal(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_html_in_terminal). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal({col, [], ["
        "        arizona_template:html({'div', [], [arizona_template:get(x, Bindings)]})"
        "    ]}). ",
        fun
            (cross_target_nesting) -> true;
            (_) -> false
        end
    ).

cross_target_terminal_in_html(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_terminal_in_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        arizona_template:terminal({line, [], [arizona_template:get(x, Bindings)]})"
        "    ]}). ",
        fun
            (cross_target_nesting) -> true;
            (_) -> false
        end
    ).

terminal_target_marked(Config) when is_list(Config) ->
    %% A ?terminal template carries `backend => arizona_terminal`.
    Mod = compile_module(
        "-module(pt_term_marked). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {line, [green], [arizona_template:get(msg, Bindings)]}"
        "    ). "
    ),
    T = Mod:render(#{msg => ~"hi"}),
    ?assertEqual(arizona_terminal, maps:get(backend, T)).

%% A `?terminal` value interpolation is now escape-marked (`{esc, Fun}`), like
%% the HTML backend, so the terminal escaper (control-char sanitizer) runs on it
%% at the render boundary. Before, terminal values were emitted bare.
terminal_value_marked_for_escape(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_term_esc). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {col, [], [{line, [], [arizona_template:get(msg, Bindings)]}]}"
        "    ). "
    ),
    T = Mod:render(#{msg => ~"hi"}),
    ?assertMatch([{_Az, {esc, _Fun}, {pt_term_esc, 1}}], maps:get(d, T)),
    [{_, {esc, Fun}, _}] = maps:get(d, T),
    ?assertEqual(~"hi", Fun()).

%% A `?terminal` dynamic value carrying control bytes is sanitized at render, so
%% it cannot inject ANSI/OSC escape sequences into the terminal (the terminal
%% analog of XSS). ESC and BEL from the value are stripped -- neutralizing the
%% OSC-52 clipboard write -- while printable bytes and UTF-8 survive. The line's
%% own ANSI (CSI `\e[`) is framework static, not a dynamic value, so it is intact.
terminal_sanitizes_control_chars(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_term_sanitize). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {col, [], [{line, [], [arizona_template:get(msg, Bindings)]}]}"
        "    ). "
    ),
    %% 'a' ESC ]52;c;<base64> BEL 'b' -- an OSC-52 clipboard-write injection.
    %% (Erlang has no `\a` escape, so BEL 0x07 is written as an explicit byte.)
    {Output, _Snap} = arizona_render:render(Mod:render(#{msg => <<"a\e]52;c;ZXZpbA==", 7, "b">>})),
    Bin = iolist_to_binary(Output),
    ?assertEqual(nomatch, binary:match(Bin, ~"\e]")),
    ?assertEqual(nomatch, binary:match(Bin, <<7>>)),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"]52;c;ZXZpbA==")),
    %% UTF-8 multi-byte content is preserved byte-intact.
    {Output2, _} = arizona_render:render(Mod:render(#{msg => <<"café"/utf8>>})),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(Output2), <<"café"/utf8>>)).

terminal_renders_styled_ansi(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_term_styled). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {col, [], ["
        "            {line, [green], [arizona_template:get(msg, Bindings)]}"
        "        ]}"
        "    ). "
    ),
    T = Mod:render(#{msg => ~"hi"}),
    {Output, _Snap} = arizona_render:render(T),
    %% green SGR (\e[32m), the dynamic text, then the line's reset+newline
    %% (\e[0m\n) followed by the col's reset (\e[0m).
    ?assertEqual(~"\e[32mhi\e[0m\n\e[0m", iolist_to_binary(Output)).

terminal_custom_sgr_attr(Config) when is_list(Config) ->
    %% {sgr, Escape} emits a raw escape verbatim (escape hatch for custom codes).
    Mod = compile_module(
        "-module(pt_term_sgr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [{sgr, <<\"\\e[1m\">>}], [arizona_template:get(msg, Bindings)]}"
        "    ). "
    ),
    T = Mod:render(#{msg => ~"x"}),
    {Output, _Snap} = arizona_render:render(T),
    ?assertEqual(~"\e[1mx\e[0m", iolist_to_binary(Output)).

terminal_fg_attr(Config) when is_list(Config) ->
    %% {fg, Index} selects a 256-colour palette entry.
    Mod = compile_module(
        "-module(pt_term_fg). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [{fg, <<\"208\">>}], [arizona_template:get(msg, Bindings)]}"
        "    ). "
    ),
    T = Mod:render(#{msg => ~"x"}),
    {Output, _Snap} = arizona_render:render(T),
    ?assertEqual(~"\e[38;5;208mx\e[0m", iolist_to_binary(Output)).

terminal_unknown_style_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_term_bad_style). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [grween], [arizona_template:get(x, Bindings)]}"
        "    ). ",
        fun
            ({render_reject, _}) -> true;
            (_) -> false
        end
    ).

%% A static `false` emits nothing, but a misspelled style is still a typo: it is
%% rejected like the `true` form, not dropped silently.
terminal_unknown_false_style_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_term_bad_false_style). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [{bolld, false}], [arizona_template:get(x, Bindings)]}"
        "    ). ",
        fun
            ({render_reject, _}) -> true;
            (_) -> false
        end
    ).

terminal_unknown_attr_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_term_bad_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [{class, <<\"box\">>}], [arizona_template:get(x, Bindings)]}"
        "    ). ",
        fun
            ({render_reject, _}) -> true;
            (_) -> false
        end
    ).

%% The terminal backend rejects an unknown attribute name loudly but used to accept
%% ANY tag as a transparent container, so a typo'd tag silently lost its meaning:
%% `{'BR', [], []}` emitted a style reset instead of a newline, and `{'Line', ...}`
%% dropped the newline the author asked for. The vocabulary is closed -- reject an
%% unknown element the same way an unknown attribute is rejected.
terminal_unknown_tag_rejected(Config) when is_list(Config) ->
    Bad = [
        {"pt_term_bad_tag", "{'div', [], [arizona_template:get(x, Bindings)]}"},
        %% The exact silent-typo shapes: a terminal tag in the wrong case.
        {"pt_term_upper_br", "{'BR', [], []}"},
        {"pt_term_upper_line", "{'Line', [], [arizona_template:get(x, Bindings)]}"}
    ],
    lists:foreach(
        fun({Mod, Elem}) ->
            assert_parse_error(
                lists:flatten([
                    "-module(",
                    Mod,
                    "). -export([render/1]). render(Bindings) -> ",
                    "arizona_template:terminal({col, [], [",
                    Elem,
                    "]}). "
                ]),
                fun
                    ({render_reject, _}) -> true;
                    (_) -> false
                end
            )
        end,
        Bad
    ).

%% Control: every documented tag still compiles and keeps its emission -- `line`
%% resets and breaks, `br` breaks, the rest are transparent containers.
terminal_known_tags_accepted(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_term_tags_ok). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal({col, [], ["
        "        {row, [], [{text, [], [<<\"a\">>]}, {span, [], [<<\"b\">>]}]}, "
        "        {line, [], [<<\"c\">>]}, "
        "        {br, [], []}"
        "    ]}). "
    ),
    {Output, _Snap} = arizona_render:render(Mod:render(#{})),
    ?assertEqual(~"a\e[0mb\e[0m\e[0mc\e[0m\n\n\e[0m", iolist_to_binary(Output)).

terminal_event_command_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_term_bad_cmd). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [{az_click, arizona_js:push_event(<<\"go\">>)}], "
        "               [arizona_template:get(x, Bindings)]}"
        "    ). ",
        fun
            ({render_reject, _}) -> true;
            (_) -> false
        end
    ).

terminal_dynamic_attr_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_term_dyn_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {text, [{fg, arizona_template:get(color, Bindings)}], "
        "               [arizona_template:get(x, Bindings)]}"
        "    ). ",
        fun
            ({render_reject, _}) -> true;
            (_) -> false
        end
    ).

terminal_each_renders(Config) when is_list(Config) ->
    %% ?each inside ?terminal compiles each item with the terminal backend and
    %% renders one ANSI line per list element.
    Mod = compile_module(
        "-module(pt_term_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal({col, [], ["
        "        arizona_template:each("
        "            fun(Item) -> {line, [], [Item]} end,"
        "            arizona_template:get(items, Bindings))"
        "    ]}). "
    ),
    T = Mod:render(#{items => [~"a", ~"b"]}),
    {Output, _Snap} = arizona_render:render(T),
    Bin = iolist_to_binary(Output),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"a\e[0m\n")),
    ?assertNotEqual(nomatch, binary:match(Bin, ~"b\e[0m\n")).

%% ============================================================================
%% Binding-read inlining
%% ============================================================================

%% A read hoisted into the function body still tracks per-slot: the slot closure
%% re-runs the `get` inside the dependency bracket instead of capturing a value.
inline_hoisted_text_read(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_text). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Name = arizona_template:get(name, Bindings), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    T = Mod:render(#{name => ~"Ada"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun()),
    ?assertEqual(#{name => true}, slot_deps(Fun)).

inline_hoisted_attr_read(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Cls = arizona_template:get(cls, Bindings), "
        "    arizona_template:html({'div', [{class, Cls}], [<<\"x\">>]}). "
    ),
    T = Mod:render(#{cls => ~"box"}),
    [{_Az, {attr, ~"class", Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"box", Fun()),
    ?assertEqual(#{cls => true}, slot_deps(Fun)).

%% A pure derivation over a single read (maps:get on a sub-map) tracks only the
%% top-level binding key, not the sub-key.
inline_maps_get_chain(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_chain). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    User = arizona_template:get(user, Bindings), "
        "    Name = maps:get(name, User), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    T = Mod:render(#{user => #{name => ~"Ada"}}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun()),
    ?assertEqual(#{user => true}, slot_deps(Fun)).

%% A case assigned to a variable inlines whole; runtime tracking records whichever
%% branch's reads fire.
inline_case_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_case). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Mode = arizona_template:get(mode, Bindings), "
        "    Label = case Mode of "
        "                dark -> arizona_template:get(dark, Bindings); "
        "                _ -> <<\"L\">> "
        "            end, "
        "    arizona_template:html({'p', [], [Label]}). "
    ),
    T = Mod:render(#{mode => dark, dark => ~"D"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"D", Fun()),
    ?assertEqual(#{mode => true, dark => true}, slot_deps(Fun)).

%% The interpolated expression *uses* a hoisted var (case-of-case): inlining walks
%% the whole slot expression, substituting the var in the scrutinee.
inline_case_of_case(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_caseofcase). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Label = case arizona_template:get(mode, Bindings) of "
        "                dark -> arizona_template:get(dark, Bindings); "
        "                _ -> <<\"L\">> "
        "            end, "
        "    arizona_template:html("
        "        {'p', [], [case Label of foo -> foo; _ -> bar end]}). "
    ),
    T = Mod:render(#{mode => light}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(bar, Fun()),
    ?assertEqual(#{mode => true}, slot_deps(Fun)).

%% An opaque call that reads bindings inlines whole; the inner get fires in-bracket.
inline_opaque_call(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_opaque). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Name = string:uppercase(arizona_template:get(raw, Bindings)), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    T = Mod:render(#{raw => ~"ada"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"ADA", Fun()),
    ?assertEqual(#{raw => true}, slot_deps(Fun)).

%% A derivation that does NOT reach a get is left captured (not inlined), so a
%% side effect is evaluated once -- repeated calls return the same value.
inline_side_effect_not_inlined(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_noinline). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Ref = erlang:unique_integer([monotonic]), "
        "    arizona_template:html({'p', [], [Ref]}). "
    ),
    T = Mod:render(#{}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(Fun(), Fun()),
    ?assertEqual(#{}, slot_deps(Fun)).

%% End-to-end regression: a hoisted read now produces an op on diff (before the
%% fix the slot was frozen and diff returned []).
inline_diff_updates(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_diff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Name = arizona_template:get(name, Bindings), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    B0 = #{name => ~"Ada"},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{name => ~"Grace"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% The inlined dep is scoped: a change to an unrelated key skips the slot.
inline_unrelated_key_skipped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_scoped). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Name = arizona_template:get(name, Bindings), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    B0 = #{name => ~"Ada", other => 1},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{name => ~"Ada", other => 2},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% A live view may hoist the root id; validate_live_root resolves it through the
%% inline map, and the id slot tracks `id`.
inline_hoisted_root_id(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_rootid). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {B, #{}}. "
        "render(Bindings) -> "
        "    Id = arizona_template:get(id, Bindings), "
        "    arizona_template:html({'div', [{id, Id}], [<<\"x\">>]}). "
    ),
    T = Mod:render(#{id => ~"v1"}),
    [IdFun] = [F || {_Az, {attr, ~"id", F}, _Loc} <- maps:get(d, T)],
    ?assertEqual(~"v1", IdFun()),
    ?assertEqual(#{id => true}, slot_deps(IdFun)).

%% A variable bound as the whole body of every branch of a statement-form case is
%% lifted to value form and inlined, so it tracks the scrutinee and branch reads.
inline_branch_bound_var(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_branchbound). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    case arizona_template:get(mode, Bindings) of "
        "        dark -> X = arizona_template:get(a, Bindings); "
        "        _ -> X = arizona_template:get(b, Bindings) "
        "    end, "
        "    arizona_template:html({'p', [], [X]}). "
    ),
    T = Mod:render(#{mode => dark, a => ~"A"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"A", Fun()),
    ?assertEqual(#{mode => true, a => true}, slot_deps(Fun)).

%% Regression: an inlined match underscored to `_Foo` must not collide with an
%% existing `_Foo` (a discarded side-effect bind) -- that would trip erl_lint's
%% match_underscore_var and fail the warnings_as_errors build. The read still tracks.
inline_underscore_collision(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_ucoll). "
        "-export([render/1]). "
        "side() -> ok. "
        "render(Bindings) -> "
        "    _Foo = side(), "
        "    Foo = arizona_template:get(foo, Bindings), "
        "    arizona_template:html({'p', [], [Foo]}). "
    ),
    T = Mod:render(#{foo => ~"Ada"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun()),
    ?assertEqual(#{foo => true}, slot_deps(Fun)).

%% Regression: a tail-bind case whose clause head binds a variable
%% (`{admin, Name} ->`) must NOT be lifted (the lifted+inlined copies would
%% double-bind Name -> unsafe_var). It stays statement-form: compiles, frozen slot.
inline_lifted_pattern_var(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_patvar). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    case arizona_template:get(user, Bindings) of "
        "        {admin, Name} -> Label = Name; "
        "        _ -> Label = arizona_template:get(guest, Bindings) "
        "    end, "
        "    arizona_template:html({'p', [], [Label]}). "
    ),
    T = Mod:render(#{user => {admin, ~"Ada"}}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun()),
    %% Not lifted -> captured -> frozen (residue).
    ?assertEqual(#{}, slot_deps(Fun)).

%% Regression: a tail-bind case whose branch RHS contains a nested match
%% (`X = (Z = E)`) must NOT be lifted (the inlined copy would re-export Z ->
%% exported_var). It stays statement-form and compiles.
inline_lifted_nested_match(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_nestmatch). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    case arizona_template:get(mode, Bindings) of "
        "        dark -> X = (Z = arizona_template:get(a, Bindings)); "
        "        _ -> X = (Z = arizona_template:get(b, Bindings)) "
        "    end, "
        "    arizona_template:html({'p', [], [X, <<\" \">>, Z]}). "
    ),
    T = Mod:render(#{mode => dark, a => ~"A"}),
    [{_Az, {esc, FunX}, _Loc} | _] = maps:get(d, T),
    ?assertEqual(~"A", FunX()).

%% A hoisted read interpolated into an attribute *list* ({class, [<<"a ">>, Cls]}):
%% inline_vars descends into the attr-value cons; the fused dynamic composes the
%% static prefix with the read value, and tracks the key.
inline_hoisted_attr_list_read(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_attrlist). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Cls = arizona_template:get(cls, Bindings), "
        "    arizona_template:html({'div', [{class, [<<\"a \">>, Cls]}], [<<\"x\">>]}). "
    ),
    T = Mod:render(#{cls => ~"box"}),
    [{_Az, {attr, ~"class", Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual([~"a ", ~"box"], Fun()),
    ?assertEqual(#{cls => true}, slot_deps(Fun)).

%% End-to-end: a hoisted ?each *source* re-tracks on diff. The each container is a
%% per-slot closure; the source get must inline back so it re-runs in the bracket.
inline_each_hoisted_source(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_eachsrc). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Items = arizona_template:get(items, Bindings), "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(I) -> "
        "        {'li', [], [I]} end, Items)]}). "
    ),
    B0 = #{items => [~"a"]},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{items => [~"a", ~"b"]},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% Two distinct hoisted reads in sibling slots each track ONLY their own key --
%% per-slot inlining must not spill one slot's binding into the other's closure.
inline_distinct_slots_key_isolation(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_isolation). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    A = arizona_template:get(a, Bindings), "
        "    B = arizona_template:get(b, Bindings), "
        "    arizona_template:html({'div', [], [{'span', [], [A]}, {'span', [], [B]}]}). "
    ),
    T = Mod:render(#{a => ~"AA", b => ~"BB"}),
    [{_Az0, {esc, FunA}, _L0}, {_Az1, {esc, FunB}, _L1}] = maps:get(d, T),
    ?assertEqual(~"AA", FunA()),
    ?assertEqual(~"BB", FunB()),
    ?assertEqual(#{a => true}, slot_deps(FunA)),
    ?assertEqual(#{b => true}, slot_deps(FunB)).

%% A multi-clause render hoists per clause; each clause's inline map is independent.
inline_multi_clause_render(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_multiclause). "
        "-export([render/1]). "
        "render(#{mode := a} = Bindings) -> "
        "    X = arizona_template:get(x, Bindings), "
        "    arizona_template:html({'p', [], [X]}); "
        "render(#{mode := b} = Bindings) -> "
        "    Y = arizona_template:get(y, Bindings), "
        "    arizona_template:html({'span', [], [Y]}). "
    ),
    TA = Mod:render(#{mode => a, x => ~"X"}),
    [{_AzA, {esc, FunA}, _LocA}] = maps:get(d, TA),
    ?assertEqual(~"X", FunA()),
    ?assertEqual(#{x => true}, slot_deps(FunA)),
    TB = Mod:render(#{mode => b, y => ~"Y"}),
    [{_AzB, {esc, FunB}, _LocB}] = maps:get(d, TB),
    ?assertEqual(~"Y", FunB()),
    ?assertEqual(#{y => true}, slot_deps(FunB)).

%% A hoisted get_lazy/3 read inlines whole (default fun included); the dep is
%% tracked even when the key is absent and the default branch fires.
inline_get_lazy_hoisted(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_getlazy). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Name = arizona_template:get_lazy(name, Bindings, fun() -> <<\"def\">> end), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    T = Mod:render(#{}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"def", Fun()),
    ?assertEqual(#{name => true}, slot_deps(Fun)).

%% A hoisted read inlined into BOTH an outer slot and a nested ?html template --
%% the post-order transform must rewrite the var at both compile sites.
inline_nested_html_shared_var(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_nested). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Outer = arizona_template:get(outer, Bindings), "
        "    arizona_template:html({'div', [], [Outer, "
        "        arizona_template:html({'span', [], [Outer]})]}). "
    ),
    T = Mod:render(#{outer => ~"O"}),
    [{_Az0, {esc, Fun1}, _Loc1}, {_Az1, Fun2, _Loc2}] = maps:get(d, T),
    ?assertEqual(~"O", Fun1()),
    ?assertEqual(#{outer => true}, slot_deps(Fun1)),
    Inner = Fun2(),
    [{_IAz, {esc, InnerFun}, _ILoc}] = maps:get(d, Inner),
    ?assertEqual(~"O", InnerFun()),
    ?assertEqual(#{outer => true}, slot_deps(InnerFun)).

%% One hoisted var in two slots: every occurrence is rewritten, each slot tracks.
inline_same_var_two_slots(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_samevar). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Name = arizona_template:get(name, Bindings), "
        "    arizona_template:html({'div', [], [Name, <<\" / \">>, Name]}). "
    ),
    T = Mod:render(#{name => ~"Ada"}),
    [{_Az0, {esc, Fun0}, _Loc0}, {_Az1, {esc, Fun1}, _Loc1}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun0()),
    ?assertEqual(~"Ada", Fun1()),
    ?assertEqual(#{name => true}, slot_deps(Fun0)),
    ?assertEqual(#{name => true}, slot_deps(Fun1)).

%% Inlining keys off the get CALL, not the bindings-arg name, so a render param
%% named anything other than `Bindings` still inlines and tracks.
inline_non_bindings_var_name(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_ctx). "
        "-export([render/1]). "
        "render(Ctx) -> "
        "    Name = arizona_template:get(name, Ctx), "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    T = Mod:render(#{name => ~"Ada"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun()),
    ?assertEqual(#{name => true}, slot_deps(Fun)).

%% Residue: a read bound through a non-bare-var (destructuring) pattern is NOT
%% inlined (scan_top_matches only picks bare-var matches). It compiles and renders
%% the first value, but the slot stays static (frozen). Documents the boundary.
inline_destructuring_not_inlined(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_destr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    {ok, Name} = {ok, arizona_template:get(name, Bindings)}, "
        "    arizona_template:html({'p', [], [Name]}). "
    ),
    T = Mod:render(#{name => ~"Ada"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"Ada", Fun()),
    ?assertEqual(#{}, slot_deps(Fun)).

%% A hoisted read used as a comprehension generator SOURCE is inlined (the
%% collapsed lc/bc/mc clause of iv/2 must preserve the comprehension tag -- this
%% uses a *binary* comprehension so a tag-hardcoding bug would be caught). The
%% slot tracks the source key; the generator variable stays local (untracked).
inline_comprehension_source(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_comp). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Names = arizona_template:get(names, Bindings), "
        "    arizona_template:html({'p', [], [<< <<N/binary>> || N <- Names >>]}). "
    ),
    T = Mod:render(#{names => [~"a", ~"b"]}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"ab", Fun()),
    ?assertEqual(#{names => true}, slot_deps(Fun)).

%% A template comprehension whose OTP-28 strict generator (`<:-`) pattern reuses a
%% hoisted read's variable name. The inline-var substitution must treat a strict
%% generator like the lazy forms -- shadowing the pattern var, not substituting the
%% read into the generator pattern (which erl_lint rejects as a hard illegal_pattern
%% error). Before the fix this fails to compile; after, it renders the fallback then
%% the list. (Non-strict compile: reusing the name inherently warns shadowed_var, which
%% is orthogonal to the illegal_pattern error the fix removes.)
inline_strict_generator_shadow(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_inline_strict_gen). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Item = arizona_template:get(fallback, Bindings), "
        "    arizona_template:html({'ul', [], ["
        "        {'li', [], [Item]},"
        "        [ Row || Item <:- arizona_template:get(items, Bindings), Row <- [Item] ]"
        "    ]}). "
    ),
    T = Mod:render(#{fallback => ~"F", items => [~"a", ~"b"]}),
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"F")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"ab")).

%% A hoisted definition is re-read inside the template so it stays tracked. A fun
%% parameter, a named fun or a generator there that rebinds a name the definition reads
%% would make that copy read the inner binding: a compile error naming both.
hoisted_var_captured_rejected(Config) when is_list(Config) ->
    Outer = "Y = <<\"outer\">>, Z = <<Y/binary, (arizona_template:get(a, Bindings))/binary>>, ",
    Cases = [
        {"each",
            Outer ++
                "arizona_template:html({ul, [], [arizona_template:each("
                "    fun(Y) -> {li, [], [Z]} end, arizona_template:get(items, Bindings))]}). ",
            {fun_param, 'Y', 'Z'}},
        {"fun",
            Outer ++
                "arizona_template:html({p, [], ["
                "    (fun(Y) -> <<Z/binary, Y/binary>> end)(<<\"i\">>)]}). ",
            {fun_param, 'Y', 'Z'}},
        {"gen", Outer ++ "arizona_template:html({p, [], [[[Z, Y] || Y <- [<<\"i\">>]]]}). ",
            {generator, 'Y', 'Z'}},
        {"bingen",
            Outer ++
                "arizona_template:html({p, [], [<< <<Z/binary, Y>> || <<Y>> <= <<\"i\">> >>]}). ",
            {generator, 'Y', 'Z'}},
        {"zip",
            Outer ++
                "arizona_template:html({p, [], [[[Z, Y] || Y <- [<<\"i\">>] && _W <- [1]]]}). ",
            {generator, 'Y', 'Z'}},
        {"named",
            "Self = <<\"outer\">>, "
            "Z = <<Self/binary, (arizona_template:get(a, Bindings))/binary>>, "
            "arizona_template:html({p, [], ["
            "    (fun Self(0) -> Z; Self(N) -> Self(N - 1) end)(1)]}). ", {named_fun, 'Self', 'Z'}},
        {"chain",
            "Y = <<\"outer\">>, "
            "A = <<(arizona_template:get(a, Bindings))/binary, Y/binary>>, "
            "B = <<A/binary, \"!\">>, "
            "arizona_template:html({p, [], [(fun(Y) -> <<B/binary, Y/binary>> end)(<<\"i\">>)]}). ",
            {fun_param, 'Y', 'A'}}
    ],
    [
        assert_parse_error(
            "-module(pt_captured_" ++ Name ++ "). -export([render/1]). render(Bindings) -> " ++
                Body,
            fun(R) -> R =:= {hoisted_var_captured, Kind, Binder, Hoisted} end
        )
     || {Name, Body, {Kind, Binder, Hoisted}} <- Cases
    ],
    ok.

hoisted_var_captured_message(Config) when is_list(Config) ->
    Message = arizona_parse_transform:format_error({hoisted_var_captured, fun_param, 'Y', 'Z'}),
    ?assertNotEqual(
        nomatch, string:find(Message, "fun parameter Y shadows the Y that hoisted Z reads")
    ).

%% A map key or a binary size in a pattern reads a variable rather than binding it,
%% so a hoisted definition reading that variable stays inlined and tracked.
hoisted_var_pattern_reads_not_captured(Config) when is_list(Config) ->
    MapKey = compile_module(
        "-module(pt_captured_mapkey). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Key = arizona_template:get(sort, Bindings), "
        "    Label = atom_to_binary(Key), "
        "    arizona_template:html({p, [], ["
        "        [[Label, V] || #{Key := V} <- arizona_template:get(rows, Bindings)],"
        "        (fun(#{Key := V}) -> [Label, V] end)(arizona_template:get(row, Bindings))"
        "    ]}). "
    ),
    B0 = #{sort => name, rows => [#{name => ~"r1"}], row => #{name => ~"f1"}},
    B1 = B0#{rows => [#{name => ~"r2"}], row => #{name => ~"f2"}},
    assert_captured_tracks(MapKey, B0, B1, [~"namer1", ~"namef1"], [~"namer2", ~"namef2"]),
    BinSize = compile_module(
        "-module(pt_captured_binsize). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Y = 8, "
        "    Z = integer_to_binary(Y + arizona_template:get(a, Bindings)), "
        "    arizona_template:html({p, [], ["
        "        [[Z, <<\"|\">>, integer_to_binary(X)] || <<X:Y>> <= <<7>>],"
        "        (fun(<<X:Y>>) -> [Z, <<\"/\">>, integer_to_binary(X)] end)(<<7>>)"
        "    ]}). "
    ),
    assert_captured_tracks(BinSize, #{a => 2}, #{a => 3}, [~"10|7", ~"10/7"], [~"11|7", ~"11/7"]).

%% A name bound inside the hoisted definition (its own generator) is not one it reads.
hoisted_var_internal_binder_not_captured(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_captured_internal). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Lbl = iolist_to_binary([Y || Y <- arizona_template:get(xs, Bindings)]), "
        "    arizona_template:html({p, [], ["
        "        (fun(Y) -> <<Lbl/binary, Y/binary>> end)(<<\"!\">>)]}). "
    ),
    assert_captured_tracks(Mod, #{xs => [~"a", ~"b"]}, #{xs => [~"c"]}, [~"ab!"], [~"c!"]).

%% A zip generator's patterns bind like any generator's: a hoisted name used as one is
%% not substituted into it, and its source sees the enclosing scope.
hoisted_var_zip_pattern_binds(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_captured_zip_pattern). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Z = arizona_template:get(a, Bindings), "
        "    arizona_template:html({p, [], [Z, "
        "        [[Z, integer_to_binary(W)] || Z <- [<<\"x\">>] && W <- [1]]]}). "
    ),
    assert_captured_tracks(Mod, #{a => ~"A"}, #{a => ~"B"}, [~"-->A<", ~"-->x1<"], [~"B"]).

assert_captured_tracks(Mod, B0, B1, Shown, New) ->
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0))),
    lists:foreach(fun(S) -> ?assertNotEqual(nomatch, binary:match(SSR, S)) end, Shown),
    {_Live, Snap, _Views} = arizona_render:render(Mod:render(B0), #{}),
    Changed = maps:from_keys(maps:keys(B1), true),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(Mod:render(B1), Snap, #{}, Changed),
    OpsBin = term_to_binary(Ops),
    lists:foreach(fun(N) -> ?assertNotEqual(nomatch, binary:match(OpsBin, N)) end, New).

%% A hoisted `?html(...)` whose body reads a binding, used in a content slot:
%% the spliced RHS must be compiled (not left as a raw runtime-stub call --
%% before the fix rendering crashed with parse_transform_not_applied) and the
%% slot must track the read inside, so a change to it produces a diff op.
inline_hoisted_html_renders_and_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_hoisthtml). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Header = arizona_template:html("
        "        {'h1', [], [arizona_template:get(title, Bindings)]}), "
        "    arizona_template:html({'div', [], [Header]}). "
    ),
    B0 = #{title => ~"T1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<h1")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"T1")),
    B1 = #{title => ~"T2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A two-level hoist chain: an inner hoisted `?html` spliced into an outer
%% hoisted `?html`, spliced into the root template. Both levels compile and the
%% inner read still reaches the root slot's deps.
inline_hoisted_html_nested_chain(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_hoistchain). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Inner = arizona_template:html("
        "        {'em', [], [arizona_template:get(x, Bindings)]}), "
        "    Outer = arizona_template:html({'p', [], [Inner]}), "
        "    arizona_template:html({'div', [], [Outer]}). "
    ),
    B0 = #{x => ~"X1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<em")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"X1")),
    B1 = #{x => ~"X2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A hoisted `?each(...)` used in a content slot compiles (before the fix the
%% raw call hit the runtime stub) and its source read fires at slot eval, so an
%% items change produces a diff op.
inline_hoisted_each_renders_and_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_hoisteach). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Items = arizona_template:each(fun(I) -> {'li', [], [I]} end, "
        "                                  arizona_template:get(items, Bindings)), "
        "    arizona_template:html({'ul', [], [Items]}). "
    ),
    B0 = #{items => [~"a"]},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<li")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"a")),
    B1 = #{items => [~"a", ~"b"]},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A hoisted `?stateless(atom, Props)` (atom sugar): the spliced RHS must go
%% through the sugar rewrite (before the fix the raw atom form died with
%% function_clause in arizona_template:stateless/2), and the props read fires
%% at slot eval, so a prop-binding change produces a diff op.
inline_hoisted_stateless_atom_renders_and_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_hoistsless). "
        "-export([render/1]). "
        "-export([card/1]). "
        "card(Props) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Props)]}). "
        "render(Bindings) -> "
        "    Card = arizona_template:stateless(card, "
        "        #{x => arizona_template:get(x, Bindings)}), "
        "    arizona_template:html({'div', [], [Card]}). "
    ),
    B0 = #{x => ~"PX1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"PX1")),
    B1 = #{x => ~"PX2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% Regression guard: a hoisted `?html` used as a whole `?each` callback body
%% (`fun(_I) -> Row end`) already worked pre-fix via the raw-wrapper unwrap and
%% must keep both rendering AND the per-item `single_root` flag (positional
%% list patching) -- a fix that splices an already-compiled template map here
%% would silently drop the flag.
each_param_rename_avoids_shadow_warning(Config) when is_list(Config) ->
    %% The per-item fun is inlined into the CALLER, so its parameters bind in the
    %% caller's scope. An item parameter named `Bindings` -- which a callback
    %% reading the item with get/2 is forced to use, since a tracked read is
    %% rejected in a scope without it -- shadowed the caller's `Bindings`, and the
    %% warning named the callee's head rather than the caller. The parameters are
    %% alpha-renamed to avoid that; compile_module_strict compiles
    %% warnings_as_errors, so a regression fails here rather than warning quietly.
    %% `_` and `_I` cover the two ignore forms: `_` binds nothing and must be left
    %% alone, `_I` must keep its underscore or the rename creates an unused_var.
    Mod = compile_module_strict(
        "-module(pt_each_param_rename). "
        "-export([shadow/1, ignored/1, underscored/1]). "
        "shadow(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each("
        "        fun(Bindings) -> {'li', [], [arizona_template:get(nm, Bindings)]} end, "
        "        arizona_template:get(items, Bindings))]}). "
        "ignored(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each("
        "        fun(_) -> {'li', [], [arizona_template:get(x, Bindings)]} end, "
        "        arizona_template:get(items, Bindings))]}). "
        "underscored(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each("
        "        fun(_I) -> {'li', [], [arizona_template:get(x, Bindings)]} end, "
        "        arizona_template:get(items, Bindings))]}). "
    ),
    %% The item's bindings still win over a same-named caller binding.
    Shadowed = iolist_to_binary(
        arizona_render:render_to_iolist(
            Mod:shadow(#{nm => ~"caller", items => [#{nm => ~"ada"}, #{nm => ~"bob"}]})
        )
    ),
    ?assertNotEqual(nomatch, binary:match(Shadowed, ~"ada")),
    ?assertNotEqual(nomatch, binary:match(Shadowed, ~"bob")),
    ?assertEqual(nomatch, binary:match(Shadowed, ~"caller")),
    %% An ignored item parameter leaves the caller's bindings reachable.
    Ignored = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:ignored(#{x => ~"cx", items => [1, 2]}))
    ),
    ?assertNotEqual(nomatch, binary:match(Ignored, ~"cx")),
    Underscored = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:underscored(#{x => ~"ux", items => [1, 2]}))
    ),
    ?assertNotEqual(nomatch, binary:match(Underscored, ~"ux")).

slot_tracks_only_reads_in_its_own_closure(Config) when is_list(Config) ->
    %% Pins both halves of the invariant, because only the contrast is
    %% informative. `Hoisted` reaches its slot through a bare variable, so the
    %% transform inlines a fresh read into the closure and the slot tracks.
    %% `Derived` arrives through a map pattern over a computed value, so the
    %% closure captures a plain term, reads nothing, and the slot is frozen after
    %% SSR -- silently, which is what makes it read as a diff bug. If the second
    %% assertion ever starts failing the limitation was fixed, and the docs in
    %% docs/architecture.md ("The invariant") need to change with it.
    Mod = compile_module_strict(
        "-module(pt_slot_closure_reads). "
        "-export([hoisted/1, derived/1, side/1]). "
        "hoisted(Bindings) -> "
        "    Instrument = arizona_template:get(instrument, Bindings), "
        "    arizona_template:html({'div', [{class, Instrument}], [<<\"x\">>]}). "
        "derived(Bindings) -> "
        "    Instrument = arizona_template:get(instrument, Bindings), "
        "    #{side := Side} = side(Instrument), "
        "    arizona_template:html({'div', [{class, Side}], [<<\"x\">>]}). "
        "side(<<\"a\">>) -> #{side => <<\"buy\">>}; "
        "side(_) -> #{side => <<\"sell\">>}. "
    ),
    B0 = #{instrument => ~"a"},
    B1 = #{instrument => ~"b"},
    Changed = compute_changed(B0, B1),
    {_H1, Snap1, V1} = arizona_render:render(Mod:hoisted(B0), #{}),
    {HoistedOps, _, _} = arizona_diff:diff(Mod:hoisted(B1), Snap1, V1, Changed),
    ?assertNotEqual([], HoistedOps),
    {_H2, Snap2, V2} = arizona_render:render(Mod:derived(B0), #{}),
    {DerivedOps, _, _} = arizona_diff:diff(Mod:derived(B1), Snap2, V2, Changed),
    ?assertEqual([], DerivedOps).

inline_hoisted_html_each_body_single_root(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_hoisteachbody). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Row = arizona_template:html("
        "        {'li', [], [arizona_template:get(x, Bindings)]}), "
        "    arizona_template:html({'ul', [], [arizona_template:each("
        "        fun(_I) -> Row end, arizona_template:get(items, Bindings))]}). "
    ),
    T = Mod:render(#{x => ~"X", items => [1, 2]}),
    [{_Az, EachFun, _Loc}] = maps:get(d, T),
    ?assertMatch(
        #{t := 0, template := #{single_root := true}},
        EachFun()
    ),
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<li")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"X")).

%% The hoisted-template tracking is scoped: a change to an unrelated key still
%% skips the slot carrying the spliced template.
inline_hoisted_html_unrelated_key_skipped(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_inline_hoistscoped). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Header = arizona_template:html("
        "        {'h1', [], [arizona_template:get(title, Bindings)]}), "
        "    arizona_template:html({'div', [], [Header]}). "
    ),
    B0 = #{title => ~"T", other => 1},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{title => ~"T", other => 2},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% ============================================================================
%% Local element-returning helper calls inlined at the template call site
%% ============================================================================

%% A 0-arity local helper returning a bare element tuple, called in a content
%% slot, inlines: before the fix it compiled clean and crashed at first render
%% with bad_template_value. The element flattens into the outer statics.
helper_zero_arity_inlines(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_zero). "
        "-export([render/1]). "
        "brand() -> {'svg', [{width, <<\"24\">>}], [<<\"logo\">>]}. "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [], [brand()]}). "
    ),
    T = Mod:render(#{}),
    ?assert(
        lists:any(
            fun(S) -> binary:match(S, ~"<svg") =/= nomatch end,
            maps:get(s, T)
        )
    ),
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"logo")).

%% A helper with args: the args are ordinary expressions substituted into the
%% body, so a `?get` at the call site lands in the slot bracket and tracks.
helper_with_args_renders_and_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_args). "
        "-export([render/1]). "
        "alert(Kind, Msg) -> {'p', [{class, Kind}], [Msg]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        alert(<<\"warn\">>, arizona_template:get(msg, Bindings))"
        "    ]}). "
    ),
    B0 = #{msg => ~"M1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"class=\"warn\"")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"M1")),
    B1 = #{msg => ~"M2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% The reactivity core: a `?get` INSIDE the helper body (via a bindings param)
%% compiles into the enclosing slot's dependency bracket, so a change to it
%% re-renders the slot -- discriminated by the op, not just the render.
helper_body_read_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_bodyread). "
        "-export([render/1]). "
        "header(B) -> {'h1', [], [arizona_template:get(title, B)]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [header(Bindings)]}). "
    ),
    B0 = #{title => ~"T1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<h1")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"T1")),
    B1 = #{title => ~"T2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A whole-body `?html(...)` wrapper unwraps exactly as ?each callbacks do: the
%% element flattens into the outer statics (pre-fix it rendered as a runtime
%% nested template) and a body read tracks (pre-fix the slot froze -- the
%% documented raw-call freeze this feature lifts for inlineable helpers).
helper_whole_body_html_unwraps_and_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_wrapped). "
        "-export([render/1]). "
        "-export([card/1]). "
        "card(B) -> "
        "    arizona_template:html("
        "        {'section', [], [arizona_template:get(x, B)]}). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [card(Bindings)]}). "
    ),
    B0 = #{x => ~"X1"},
    T = Mod:render(B0),
    ?assert(
        lists:any(
            fun(S) -> binary:match(S, ~"<section") =/= nomatch end,
            maps:get(s, T)
        )
    ),
    {_HTML, Snap0, V0} = arizona_render:render(T, #{}),
    B1 = #{x => ~"X2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A helper body may itself call another helper: nested calls inline
%% recursively.
%% An element helper whose body binds a variable -- here through a case-clause
%% pattern, the shape that survives the single-expression-body guard -- whose name
%% the CALLER also bound. Inlining spliced the helper's binder into the caller's
%% scope, where the pattern became a match against the caller's value and raised
%% `case_clause` the first time the two differed. The helper's own variables are
%% alpha-renamed; its parameters are left to the argument substitution.
helper_body_var_shadows_caller_var(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_helper_shadow). "
        "-export([render/1]). "
        "badge(B) -> {'span', [], [case maps:get(side, B) of Side -> Side end]}. "
        "render(Bindings) -> "
        "    Side = arizona_template:get(side, Bindings), "
        "    arizona_template:html({'div', [{class, Side}], "
        "        [badge(arizona_template:get(entry, Bindings))]}). "
    ),
    T = Mod:render(#{side => ~"buy", entry => #{side => ~"sell"}}),
    {HTML, _Snap} = arizona_render:render(T),
    HTMLBin = iolist_to_binary(HTML),
    %% The caller's `side` reaches the class, the helper's reaches the span.
    ?assertMatch({_, _}, binary:match(HTMLBin, ~"class=\"buy\"")),
    ?assertMatch({_, _}, binary:match(HTMLBin, ~"sell")).

helper_nested_calls_inline(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_nested). "
        "-export([render/1]). "
        "brand() -> {'svg', [], [<<\"logo\">>]}. "
        "card(B) -> {'section', [], [brand(), arizona_template:get(x, B)]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [card(Bindings)]}). "
    ),
    {HTML, _Snap} = arizona_render:render(Mod:render(#{x => ~"X"})),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<svg")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"logo")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"X")).

%% An element-LIST body inlines as a nested-template child and its reads become
%% slot deps (via the bare nested-template leaf tracking), so it stays reactive.
helper_element_list_body_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_list). "
        "-export([render/1]). "
        "rows(B) -> "
        "    [{'p', [], [arizona_template:get(a, B)]}, {'p', [], [<<\"s\">>]}]. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [rows(Bindings)]}). "
    ),
    B0 = #{a => ~"A1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"A1")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"s")),
    B1 = #{a => ~"A2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A helper call as the whole ?each callback body inlines to the element, so
%% the per-item template compiles (pre-fix: each_body_not_element).
helper_in_each_item_body_inlines(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_eachitem). "
        "-export([render/1]). "
        "item(I) -> {'li', [], [I]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each("
        "        fun(I) -> item(I) end, arizona_template:get(items, Bindings))]}). "
    ),
    {HTML, _Snap} = arizona_render:render(Mod:render(#{items => [~"a", ~"b"]})),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<li")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"a")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"b")).

%% A same-module explicit call (`?MODULE:helper()`, here spelled literally)
%% resolves to the local body exactly like the bare call -- mirroring the
%% fun ?MODULE:row/1 ?each rule.
helper_same_module_call_inlines(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_samemod). "
        "-export([render/1]). "
        "brand() -> {'svg', [], [<<\"logo\">>]}. "
        "render(_Bindings) -> "
        "    arizona_template:html({'div', [], [pt_helper_samemod:brand()]}). "
    ),
    {HTML, _Snap} = arizona_render:render(Mod:render(#{})),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<svg")).

%% Supporting fix: a LITERAL element-list child compiles to a nested template
%% whose reads were isolated from the slot bracket, so the slot froze. The bare
%% nested-template leaf now contributes its literal reads as slot deps (same
%% principle as conditional branch-read tracking).
element_list_child_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_elem_list_child). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        [{'p', [], [arizona_template:get(x, Bindings)]},"
        "         {'p', [], [<<\"s\">>]}]"
        "    ]}). "
    ),
    B0 = #{x => ~"A"},
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{x => ~"B"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A multi-clause helper with a bare-element clause cannot be inlined (one
%% shared call site cannot select a clause) -- and pre-fix it crashed at
%% render, so it is a compile error now.
helper_multi_clause_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_helper_multi). "
        "-export([render/1]). "
        "badge(ok) -> {'span', [{class, <<\"ok\">>}], [<<\"OK\">>]}; "
        "badge(_) -> {'span', [{class, <<\"err\">>}], [<<\"ERR\">>]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], "
        "        [badge(arizona_template:get(status, Bindings))]}). ",
        fun(R) -> R =:= {helper_multi_clause, badge, 1} end
    ).

%% A guarded bare-element helper cannot keep its guard through inlining.
helper_guarded_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_helper_guarded). "
        "-export([render/1]). "
        "badge(K) when is_binary(K) -> {'span', [], [K]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], "
        "        [badge(arizona_template:get(k, Bindings))]}). ",
        fun(R) -> R =:= {helper_guarded, badge, 1} end
    ).

%% Substitution needs each parameter to be a distinct plain variable.
helper_params_not_vars_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_helper_pattern). "
        "-export([render/1]). "
        "badge({K, V}) -> {'span', [{class, K}], [V]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], "
        "        [badge(arizona_template:get(kv, Bindings))]}). ",
        fun(R) -> R =:= {helper_params_not_vars, badge, 1} end
    ).

%% Statements before the element cannot be inlined (a begin-block would leak
%% its bindings into the caller's scope).
helper_body_not_single_expr_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_helper_stmts). "
        "-export([render/1]). "
        "brand() -> "
        "    Size = <<\"24\">>, "
        "    {'svg', [{width, Size}], [<<\"logo\">>]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [brand()]}). ",
        fun(R) -> R =:= {helper_body_not_single_expr, brand, 0} end
    ).

%% A recursive element helper cannot be inlined into itself.
helper_recursive_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_helper_rec). "
        "-export([render/1]). "
        "tree() -> {'div', [], [tree()]}. "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [tree()]}). ",
        fun(R) -> R =:= {helper_recursive, tree, 0} end
    ).

%% Control: a scalar-returning local call keeps today's behavior -- the call
%% runs inside the slot closure, so its body read fires in-bracket and tracks.
%% No inlining, no rejection.
helper_scalar_body_read_still_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_scalar). "
        "-export([render/1]). "
        "-export([label/1]). "
        "label(B) -> arizona_template:get(x, B). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [label(Bindings)]}). "
    ),
    B0 = #{x => ~"A"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), ~"A")),
    B1 = #{x => ~"B"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% Control: a multi-clause SCALAR helper is untouched (no rejection) -- the
%% multi-clause error fires only when a clause returns a bare element.
helper_multi_clause_scalar_untouched(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_multiscalar). "
        "-export([render/1]). "
        "-export([label/1]). "
        "label(ok) -> <<\"OK\">>; "
        "label(_) -> <<\"ERR\">>. "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], "
        "        [label(arizona_template:get(status, Bindings))]}). "
    ),
    {HTML, _Snap} = arizona_render:render(Mod:render(#{status => ok})),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), ~"OK")).

%% Control: a multi-statement `?html`-wrapped helper is NOT inlineable and NOT
%% rejected -- it keeps today's behavior (renders as a runtime nested template;
%% body reads stay frozen, the az:with handoff exists for that pattern).
helper_wrapped_multi_statement_untouched(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_helper_wrappedstmts). "
        "-export([render/1]). "
        "-export([card/1]). "
        "card(B) -> "
        "    Cls = <<\"card\">>, "
        "    arizona_template:html("
        "        {'section', [{class, Cls}], [arizona_template:get(x, B)]}). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], [card(Bindings)]}). "
    ),
    B0 = #{x => ~"X1"},
    {HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"<section")),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"X1")),
    B1 = #{x => ~"X2"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% ============================================================================
%% Tracked get/get_lazy on a non-bindings map
%% ============================================================================

%% Reading a sub-map with the tracked accessor wrongly records the inner key as a
%% top-level dependency, so it is a compile error.
tracked_get_submap_errors(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_tg_submap). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Foo = arizona_template:get(foo, Bindings), "
        "    arizona_template:html({'p', [], [arizona_template:get(bar, Foo)]}). ",
        fun(R) -> R =:= tracked_get_on_non_bindings_map end
    ).

%% The az: alias form is flagged too.
tracked_get_submap_az_errors(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_tg_az). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Foo = az:get(foo, Bindings), "
        "    arizona_template:html({'p', [], [az:get(bar, Foo)]}). ",
        fun(R) -> R =:= tracked_get_on_non_bindings_map end
    ).

%% get_lazy on a sub-map is flagged.
tracked_get_lazy_submap_errors(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_tg_lazy). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Foo = arizona_template:get(foo, Bindings), "
        "    arizona_template:html({'p', [], "
        "        [arizona_template:get_lazy(bar, Foo, fun() -> <<>> end)]}). ",
        fun(R) -> R =:= tracked_get_on_non_bindings_map end
    ).

%% maps:get for the sub-field is the correct pattern and compiles clean.
tracked_get_maps_get_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_tg_mapsget). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Foo = arizona_template:get(foo, Bindings), "
        "    Bar = maps:get(bar, Foo), "
        "    arizona_template:html({'p', [], [Bar]}). "
    ),
    ?assert(is_map(Mod:render(#{foo => #{bar => ~"x"}}))).

%% An ?each item read uses the item fun parameter (the per-item bindings), so it
%% is not flagged.
tracked_get_each_item_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_tg_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], [arizona_template:each(fun(Item) -> "
        "        {'li', [], [arizona_template:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings))]}). "
    ),
    ?assert(is_map(Mod:render(#{items => [#{name => ~"a"}]}))).

%% An alias chain of the bindings is not flagged.
tracked_get_alias_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_tg_alias). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    B = Bindings, "
        "    C = B, "
        "    arizona_template:html({'p', [], [arizona_template:get(x, C)]}). "
    ),
    ?assert(is_map(Mod:render(#{x => ~"y"}))).

%% A literal-map second argument (a non-variable) is never flagged.
tracked_get_literal_map_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_tg_litmap). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Real = arizona_template:get(real, Bindings), "
        "    V = arizona_template:get(k, #{k => v}), "
        "    arizona_template:html({'p', [], [Real, <<\" \">>, V]}). "
    ),
    ?assert(is_map(Mod:render(#{real => ~"R"}))).

%% A renamed bindings parameter (not literally `Bindings`) is in scope and passes.
tracked_get_renamed_param_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_tg_ctx). "
        "-export([render/1]). "
        "render(Ctx) -> "
        "    arizona_template:html({'p', [], [arizona_template:get(name, Ctx)]}). "
    ),
    ?assert(is_map(Mod:render(#{name => ~"Ada"}))).

%% A var bound by a with/2 projection is bindings-like: reading it with get/2 is not
%% flagged (the projection already tracked the key on the enclosing slot). The slot
%% tracks `x` (not just compiles): the hoisted `Sub` is inlined back into the bracket.
tracked_get_with_projection_ok(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_tg_withproj). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Sub = arizona_template:with([x], Bindings), "
        "    arizona_template:html({'p', [], [arizona_template:get(x, Sub)]}). "
    ),
    T = Mod:render(#{x => ~"y"}),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, T),
    ?assertEqual(~"y", Fun()),
    ?assertEqual(#{x => true}, slot_deps(Fun)).

%% with/2 is itself a tracking accessor, so projecting a non-bindings sub-map is the
%% same footgun as get/get_lazy and is flagged.
tracked_get_with_submap_errors(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_tg_withsub). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Foo = arizona_template:get(foo, Bindings), "
        "    Sub = arizona_template:with([bar], Foo), "
        "    arizona_template:html({'p', [], [arizona_template:get(bar, Sub)]}). ",
        fun(R) -> R =:= tracked_get_on_non_bindings_map end
    ).

%% A binding reached through a non-bare-var alias (a `case`, `begin`, `maps:merge`, ...)
%% is intentionally flagged: alias_closure only proves bindings-likeness through `V = W`
%% and `V = with(_, W)`, so a `case`-aliased binding is indistinguishable from a sub-map.
%% This documents the limitation and the escape hatch (alias directly with `B = Bindings`,
%% per the format_error message). The value still renders correctly via that escape.
tracked_get_case_aliased_bindings_flagged(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_tg_casealias). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    B = case Bindings of #{} -> Bindings end, "
        "    arizona_template:html({'p', [], [arizona_template:get(x, B)]}). ",
        fun(R) -> R =:= tracked_get_on_non_bindings_map end
    ).

%% Finding A, baseline: a child template handed off via a raw function call freezes.
%% The outer slot is `fun() -> inner(Bindings) end`; building `inner` fires no tracked
%% read at the outer level (its reads sit in inner's own closures), so the outer slot
%% captures empty deps (proven directly via slot_deps) and the diff engine skips it.
%% The frozen baseline `az:with` exists for: a raw call returning a template.
%% Note the helper is deliberately NOT inlineable (statements before its
%% whole-body ?html) -- a cleanly-inlineable single-expression helper is now
%% inlined by the element-helper feature and tracks its body reads instead.
with_handoff_freezes_without_tracking(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_with_freeze). "
        "-export([render/1, inner/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [inner(Bindings)]}). "
        "inner(B) -> "
        "    Cls = <<\"row\">>, "
        "    arizona_template:html("
        "        {'span', [{class, Cls}], [arizona_template:get(x, B)]}). "
    ),
    B0 = #{x => ~"Ada"},
    [{_Az, {esc, OuterFun}, _Loc}] = maps:get(d, Mod:render(B0)),
    ?assertEqual(#{}, slot_deps(OuterFun)),
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{x => ~"Grace"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertEqual([], Ops).

%% Finding A, fix (inline form): handing off `with([x], Bindings)` declares the outer
%% slot's dependency on `x` (proven via slot_deps), so it re-renders when `x` changes
%% (the frozen baseline above emits no op for the same change).
with_handoff_tracks_outer_slot(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_with_track). "
        "-export([render/1, inner/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [inner(arizona_template:with([x], Bindings))]}). "
        "inner(B) -> "
        "    arizona_template:html({'span', [], [arizona_template:get(x, B)]}). "
    ),
    B0 = #{x => ~"Ada"},
    [{_Az, {esc, OuterFun}, _Loc}] = maps:get(d, Mod:render(B0)),
    ?assertEqual(#{x => true}, slot_deps(OuterFun)),
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{x => ~"Grace"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% Finding A, fix (HOISTED form): the same handoff with `with` bound to a local first.
%% This is the regression guard for the rhs_reaches/2 inlining gate: without `with` in
%% that gate the hoisted `Sub` is not inlined, its tracking runs outside the slot bracket
%% (a no-op), and the outer slot freezes (slot_deps `#{}`, Ops `[]`). With the fix the
%% slot tracks `x` exactly like the inline form above.
with_handoff_hoisted_tracks_outer_slot(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_with_hoist). "
        "-export([render/1, inner/1]). "
        "render(Bindings) -> "
        "    Sub = arizona_template:with([x], Bindings), "
        "    arizona_template:html({'p', [], [inner(Sub)]}). "
        "inner(B) -> "
        "    arizona_template:html({'span', [], [arizona_template:get(x, B)]}). "
    ),
    B0 = #{x => ~"Ada"},
    [{_Az, {esc, OuterFun}, _Loc}] = maps:get(d, Mod:render(B0)),
    ?assertEqual(#{x => true}, slot_deps(OuterFun)),
    {_HTML, Snap0, V0} = arizona_render:render(Mod:render(B0), #{}),
    B1 = #{x => ~"Grace"},
    Changed = compute_changed(B0, B1),
    {Ops, _Snap1, _V1} = arizona_diff:diff(Mod:render(B1), Snap0, V0, Changed),
    ?assertNotEqual([], Ops).

%% A tracked read used in a `case` clause guard auto-tracks: the transform records the
%% guard's bindings as slot dependencies (it can't put the read in the guard, so it
%% injects a tracking touch), keeping the slot reactive. Both reads are tracked, and
%% flipping only `confirming` re-renders the slot (no freeze).
guard_case_tracked_read_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_case). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Status = arizona_template:get(status, Bindings), "
        "    Confirming = arizona_template:get(confirming, Bindings), "
        "    arizona_template:html({'p', [], ["
        "        case Status of "
        "            active when Confirming -> <<\"y\">>; "
        "            _ -> <<>> "
        "        end]}). "
    ),
    B0 = #{status => active, confirming => false},
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(B0), #{}),
    ?assertEqual([#{status => true, confirming => true}], maps:get(deps, Snap)),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(
        Mod:render(#{status => active, confirming => true}), Snap, #{}, #{confirming => true}
    ),
    ?assertNotEqual([], Ops).

%% Same auto-tracking through an `if` (its condition is a guard).
guard_if_tracked_read_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_if). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Confirming = arizona_template:get(confirming, Bindings), "
        "    arizona_template:html({'p', [], ["
        "        if Confirming -> <<\"y\">>; true -> <<>> end]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{confirming => false}), #{}),
    ?assertEqual([#{confirming => true}], maps:get(deps, Snap)).

%% A tracked var buried in a guard BIF / boolean combination is still tracked.
guard_buried_tracked_read_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_buried). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    N = arizona_template:get(n, Bindings), "
        "    arizona_template:html({'p', [], ["
        "        case ok of "
        "            _ when is_integer(N) andalso N > 5 -> <<\"y\">>; "
        "            _ -> <<>> "
        "        end]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{n => 10}), #{}),
    ?assertEqual([#{n => true}], maps:get(deps, Snap)).

%% A guarded `case` whose result is hoisted into a slot is inlined into that slot, so its
%% guard lands in the slot and auto-tracks -- the case `inline_guard_get_stays_captured`
%% used to document as a silent freeze.
guard_hoisted_case_result_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_hoist). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    N = arizona_template:get(n, Bindings), "
        "    Label = case x of _ when N > 5 -> <<\"big\">>; _ -> <<\"small\">> end, "
        "    arizona_template:html({'p', [], [Label]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{n => 10}), #{}),
    ?assertEqual([#{n => true}], maps:get(deps, Snap)).

%% Auto-tracked even when the var is ALSO read in the body. The touch is unconditional,
%% so the dep is recorded even when SSR takes the branch that does NOT read it (here N=3
%% fails the guard, so `integer_to_binary(N)` never runs -- yet `n` is still tracked).
guard_tracked_read_also_in_body_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_body). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    N = arizona_template:get(n, Bindings), "
        "    arizona_template:html({'p', [], ["
        "        case ok of "
        "            _ when N > 5 -> integer_to_binary(N); "
        "            _ -> <<>> "
        "        end]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{n => 3}), #{}),
    ?assertEqual([#{n => true}], maps:get(deps, Snap)).

%% The guard variable need not be the direct ?get: a variable transitively derived from a
%% tracked read (M = N, N = ?get(n)) auto-tracks the underlying binding `n`.
guard_transitively_derived_read_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_trans). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    N = arizona_template:get(n, Bindings), "
        "    M = N, "
        "    arizona_template:html({'p', [], ["
        "        case x of _ when M > 5 -> <<\"big\">>; _ -> <<\"small\">> end]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{n => 10}), #{}),
    ?assertEqual([#{n => true}], maps:get(deps, Snap)).

%% A guard in a content-slot fun (a fun literal applied inline) over an outer tracked var
%% is auto-tracked: the nested fun node is wrapped. (A top-level ?each callback is not
%% wrapped -- its guards are over the item param; see inline_vars/2 / compile_each.)
guard_fun_clause_tracked_read_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_fun). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    N = arizona_template:get(n, Bindings), "
        "    arizona_template:html({'p', [], ["
        "        (fun (X) when N > 5 -> X; (_) -> <<>> end)(<<\"y\">>)]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{n => 10}), #{}),
    ?assertEqual([#{n => true}], maps:get(deps, Snap)).

%% A guard inside an ATTRIBUTE-value expression (a different compile path than a content
%% slot) is auto-tracked the same way.
guard_attr_value_tracks(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_attr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Dark = arizona_template:get(dark, Bindings), "
        "    arizona_template:html({'div', [{class, "
        "        case x of _ when Dark -> <<\"d\">>; _ -> <<\"l\">> end"
        "    }], [<<\"hi\">>]}). "
    ),
    {HTML, Snap, _Views} = arizona_render:render(Mod:render(#{dark => true}), #{}),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), <<"class=\"d\"">>)),
    ?assertEqual([#{dark => true}], maps:get(deps, Snap)).

%% az-nodiff + a guard over a tracked read compiles (no error) and renders: the slot is
%% intentionally frozen, and the injected touch is a harmless no-op under nodiff.
guard_nodiff_compiles(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Show = arizona_template:get(show, Bindings), "
        "    arizona_template:html({'div', ['az-nodiff'], ["
        "        if Show -> {'p', [], [<<\"yes\">>]}; true -> <<\"\">> end]}). "
    ),
    {HTML, _Snap} = arizona_render:render(Mod:render(#{show => true})),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), <<"<p>yes</p>">>)).

%% Negative: a variable bound by the clause PATTERN (not a tracked outer read) is
%% not flagged. The slot compiles and tracks the scrutinee read.
guard_pattern_var_ok(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_patvar). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], ["
        "        case arizona_template:get(count, Bindings) of "
        "            V when V > 0 -> <<\"some\">>; "
        "            _ -> <<\"none\">> "
        "        end]}). "
    ),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, Mod:render(#{count => 3})),
    ?assertEqual(<<"some">>, Fun()),
    ?assertEqual(#{count => true}, slot_deps(Fun)).

%% Negative: a non-tracked local (no ?get behind it) in a guard is fine -- there is
%% no dependency to lose. Compiles and tracks only the scrutinee read.
guard_untracked_local_ok(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Limit = 10, "
        "    arizona_template:html({'p', [], ["
        "        case arizona_template:get(count, Bindings) of "
        "            V when V > Limit -> <<\"over\">>; "
        "            _ -> <<\"ok\">> "
        "        end]}). "
    ),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, Mod:render(#{count => 5})),
    ?assertEqual(<<"ok">>, Fun()),
    ?assertEqual(#{count => true}, slot_deps(Fun)).

%% Negative + idiomatic fix: moving both reads into the case scrutinee (pattern
%% match instead of guard) compiles and tracks BOTH dependencies.
guard_tracked_read_in_scrutinee_ok(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_scrut). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], ["
        "        case {arizona_template:get(status, Bindings), "
        "              arizona_template:get(confirming, Bindings)} of "
        "            {active, true} -> <<\"Confirm\">>; "
        "            _ -> <<>> "
        "        end]}). "
    ),
    [{_Az, {esc, Fun}, _Loc}] = maps:get(d, Mod:render(#{status => active, confirming => true})),
    ?assertEqual(<<"Confirm">>, Fun()),
    ?assertEqual(#{status => true, confirming => true}, slot_deps(Fun)).

%% Negative + the one reactive `if`: a variable bound by a tracked `case` scrutinee's
%% pattern is not a tracked read, so an `if` over it is not flagged. The scrutinee
%% read is tracked, so the slot stays reactive -- this is how `if` is used reactively
%% in a template (the bare standalone `if` over a binding cannot be, see
%% cond_if_bare_element).
guard_pattern_bound_from_tracked_case_ok(Config) when is_list(Config) ->
    Mod = compile_module_strict(
        "-module(pt_guard_pbcase). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(show, Bindings) of "
        "            Show -> if Show -> {'p', [], [<<\"yes\">>]}; true -> <<\"\">> end "
        "        end]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{show => true}), #{}),
    ?assertEqual([#{show => true}], maps:get(deps, Snap)).

%% ============================================================================
%% az scoping vs. user-authored marker lookalikes
%% ============================================================================

%% Compile-time scoping. A static text child is spliced verbatim (the documented
%% raw-HTML seam), so a page that *shows* markup carries ` az="` and `<!--az:`
%% as ordinary content. Prefixing the fingerprint must rebuild only the markers
%% the transform itself emitted and leave those bytes alone.
scope_keeps_static_marker_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_scope_text). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        <<\"write az=\\\"1\\\" and <!--az:2--> to target\">>, "
        "        arizona_template:get(v, Bindings)"
        "    ]}). "
    ),
    Tmpl = Mod:render(#{v => ~"V"}),
    Fp = maps:get(f, Tmpl),
    [S1, _S2] = maps:get(s, Tmpl),
    %% The user's bytes survive verbatim...
    ?assertNotEqual(nomatch, binary:match(S1, ~"write az=\"1\" and <!--az:2--> to target")),
    %% ...while the element `az` and the content-slot marker are still scoped.
    ?assertNotEqual(nomatch, binary:match(S1, <<"<div az=\"", Fp/binary, "-0\">">>)),
    ?assertNotEqual(nomatch, binary:match(S1, <<"<!--az:", Fp/binary, "-0-->">>)).

%% The per-item template of an `?each` is compiled and scoped by the same path,
%% so an item whose static text shows markup is preserved there too.
scope_keeps_static_marker_text_in_each_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_scope_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], ["
        "        arizona_template:each(fun(Item) -> "
        "            {'li', [], [<<\"write az=\\\"1\\\" here \">>, Item]} "
        "        end, arizona_template:get(items, Bindings))"
        "    ]}). "
    ),
    {HTML, _Snap} = arizona_render:render(Mod:render(#{items => [~"a", ~"b"]})),
    HTMLBin = iolist_to_binary(HTML),
    ?assertEqual(2, length(binary:matches(HTMLBin, ~"write az=\"1\" here"))).

%% Runtime scoping. A conditional branch compiles to a nested template whose
%% statics are re-scoped by the enclosing slot `az` when the slot is evaluated.
%% The branch's own marker gains the slot prefix; its static text does not.
scope_slot_keeps_static_marker_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_scope_slot). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(flag, Bindings) of "
        "            true -> {'p', [], [<<\"write az=\\\"9\\\" here \">>, "
        "                               arizona_template:get(v, Bindings)]}; "
        "            false -> <<>> "
        "        end"
        "    ]}). "
    ),
    {HTML, Snap} = arizona_render:render(Mod:render(#{flag => true, v => ~"V"})),
    HTMLBin = iolist_to_binary(HTML),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, ~"write az=\"9\" here")),
    %% The nested snapshot's fingerprint carries the slot prefix, and every
    %% marker in its statics carries the same scoped fingerprint.
    [{SlotAz, #{f := NestedFp}}] = maps:get(d, Snap),
    ?assertEqual(<<SlotAz/binary, "-">>, binary:part(NestedFp, 0, byte_size(SlotAz) + 1)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<p az=\"", NestedFp/binary, "-0\">">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<!--az:", NestedFp/binary, "-0-->">>)).

%% The same runtime pass namespaces a repeated stateless child: both instances
%% keep their static text intact and still get distinct `az` targets (the whole
%% point of the scoping).
scope_repeated_stateless_keeps_static_marker_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_scope_stateless). "
        "-export([render/1, row/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        arizona_template:stateless(fun row/1, #{n => arizona_template:get(a, Bindings)}), "
        "        arizona_template:stateless(fun row/1, #{n => arizona_template:get(b, Bindings)})"
        "    ]}). "
        "row(Bindings) -> "
        "    arizona_template:html({'span', [], [<<\"write az=\\\"9\\\" here \">>, "
        "                                        arizona_template:get(n, Bindings)]}). "
    ),
    {HTML, Snap} = arizona_render:render(Mod:render(#{a => ~"A", b => ~"B"})),
    HTMLBin = iolist_to_binary(HTML),
    ?assertEqual(2, length(binary:matches(HTMLBin, ~"write az=\"9\" here"))),
    [{_Az1, #{f := Fp1}}, {_Az2, #{f := Fp2}}] = maps:get(d, Snap),
    ?assertNotEqual(Fp1, Fp2),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<span az=\"", Fp1/binary, "-0\">">>)),
    ?assertNotEqual(nomatch, binary:match(HTMLBin, <<"<span az=\"", Fp2/binary, "-0\">">>)).

%% Parse `Source` into forms, untransformed.
parse_forms(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- split_forms(Tokens)
    ].

%% Like compile_module/1 but compiles with warnings_as_errors, matching the
%% project's real erl_opts. Needed because unused-variable / match_underscore_var
%% regressions surface only as warnings, which the plain [return_errors] path drops.
compile_module_strict(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    Forms = split_forms(Tokens),
    ParsedForms = [
        begin
            {ok, F} = erl_parse:parse_form(Toks),
            F
        end
     || Toks <- Forms
    ],
    TransformedForms = arizona_parse_transform:parse_transform(ParsedForms, []),
    {ok, Mod, Bin} = compile:forms(TransformedForms, [return_errors, warnings_as_errors]),
    {module, Mod} = code:load_binary(Mod, "", Bin),
    Mod.

%% Evaluate a slot closure under a dependency bracket (mirrors arizona_eval) and
%% return the captured deps.
slot_deps(Fun) ->
    erlang:put('$arizona_deps', #{}),
    Fun(),
    erlang:erase('$arizona_deps').

%% Mirrors arizona_live:compute_changed/2 for unit tests.
compute_changed(OldBindings, NewBindings) ->
    maps:filter(
        fun(K, V) ->
            case OldBindings of
                #{K := V} -> false;
                #{} -> true
            end
        end,
        NewBindings
    ).

%% ============================================================================
%% az_attrs -- the `az-*` names shipped to the client
%% ============================================================================

%% A dynamic value is recorded when a builder call is visible anywhere in the
%% expression -- literal args, dynamic args, a list of commands, a branch of a
%% conditional -- because the builders are the only producers of effect tuples,
%% so the call is proof the value is a command.
az_attrs_records_command_names(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_cmd). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ["
        "            {az_click, arizona_js:push_event(<<\"c\">>)},"
        "            {<<\"az-sl-change\">>, arizona_js:push_event(az:get(ev, Bindings))},"
        "            {az_dblclick,"
        "                [arizona_js:push_event(<<\"d\">>), arizona_js:toggle(<<\"#m\">>)]},"
        "            {az_mouseenter, case az:get(os, Bindings) of"
        "                true -> arizona_os:focus();"
        "                false -> arizona_android:push_event(<<\"m\">>)"
        "            end},"
        "            'az-prevent-default'"
        "        ], [<<\"x\">>]}"
        "    ). "
    ),
    ?assertEqual(
        [
            <<"az-click">>,
            <<"az-dblclick">>,
            <<"az-mouseenter">>,
            <<"az-prevent-default">>,
            <<"az-sl-change">>
        ],
        az_attrs_of(Mod)
    ).

%% Everything a command cannot be is skipped: a static binary is app data (an
%% effect is always a call, never a literal), a bare or boolean attribute has no
%% value to hold a command, and an opaque dynamic with no builder call in sight
%% is indistinguishable from app data -- recording it would delegate the event
%% type and hand `[1,2,3]` to the command interpreter. Opaque dynamics that DO
%% evaluate to commands are observed at render time instead
%% (arizona_event_attrs), never guessed here.
az_attrs_skips_unproven_values(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_data). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ["
        "            {az_select, <<\"[1,2,3]\">>},"
        "            {az_meta, az:get(ids, Bindings)},"
        "            az_expanded,"
        "            {az_open, true},"
        "            {az_click, arizona_js:push_event(<<\"c\">>)}"
        "        ], [<<\"x\">>]}"
        "    ). "
    ),
    ?assertEqual([<<"az-click">>], az_attrs_of(Mod)).

%% ...except `az-prevent-default`, a directive whose value the client ignores. Left
%% out of the set, the client registers wheel/touch listeners passive and silently
%% disarms the `preventDefault` the attribute asked for -- so it is recorded in
%% every form, dynamic value included.
az_attrs_keeps_prevent_default_with_a_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_pd). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ["
        "            {az_prevent_default, <<\"true\">>},"
        "            {az_wheel, arizona_js:push_event(<<\"w\">>)}"
        "        ], [<<\"x\">>]}"
        "    ). "
    ),
    ?assertEqual([<<"az-prevent-default">>, <<"az-wheel">>], az_attrs_of(Mod)),
    DynMod = compile_module(
        "-module(pt_az_pd_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{az_prevent_default, az:get(pd, Bindings)}], [<<\"x\">>]}"
        "    ). "
    ),
    ?assertEqual([<<"az-prevent-default">>], az_attrs_of(DynMod)).

%% The injected `az-view` marker lands in the DOM but not in the declared set:
%% it is framework plumbing, not an event type, and it would ride every connect
%% frame of every app as noise.
az_attrs_skips_injected_az_view(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_view_unrecorded). "
        "-behaviour(arizona_stateful). "
        "-export([mount/1, render/1]). "
        "mount(B) -> {B, #{}}. "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [{id, az:get(id, Bindings)}], [<<\"hi\">>]}"
        "    ). "
    ),
    T = Mod:render(#{id => <<"v">>}),
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertNotEqual(nomatch, binary:match(StaticsBin, <<"az-view">>)),
    ?assertEqual([], az_attrs_of(Mod)).

az_attrs_of(Mod) ->
    proplists:get_value(arizona_az_attrs, Mod:module_info(attributes), []).

%% The component graph is visible only at compile time: `?stateful`/`?stateless`
%% pass the module as DATA, so it never becomes a call and does not survive into the
%% beam. Recording it is what lets `arizona_event_attrs:all/1` walk from a route's
%% handler to every component the page can render.
az_attrs_records_component_modules(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_az_deps). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', [], ["
        "            arizona_template:stateless(other_component, row, #{}),"
        "            arizona_template:stateful(child_view, #{})"
        "        ]}"
        "    ). "
    ),
    ?assertEqual(
        [child_view, other_component],
        proplists:get_value(arizona_az_deps, Mod:module_info(attributes), [])
    ).

%% --------------------------------------------------------------------
%% nodiff_slot: a slot holding an az-nodiff element
%% --------------------------------------------------------------------

%% The server decides whether the az-nodiff element exists: hiding and showing it patch the
%% slot.
nodiff_slot_show_hide(Config) when is_list(Config) ->
    B0 = #{show => true, v => ~"a"},
    {_, S0, V0} = nodiff_render(toggle, B0),
    {Hide, S1, V1} = nodiff_diff(toggle, B0#{show => false}, S0, V0, [show]),
    ?assertMatch([[?OP_TEXT, _, <<>>]], Hide),
    {Show, _, _} = nodiff_diff(toggle, B0#{v => ~"b"}, S1, V1, [show, v]),
    ?assertMatch([[?OP_TEXT, _, #{~"d" := [_, ~"b"]}]], Show).

%% Switching between two az-nodiff elements patches the slot, whether or not the two share
%% their statics.
nodiff_slot_switch(Config) when is_list(Config) ->
    B0 = #{kind => a, v => ~"1"},
    {_, S0, V0} = nodiff_render(switch, B0),
    {AB, S1, V1} = nodiff_diff(switch, B0#{kind => b, v => ~"2"}, S0, V0, [kind, v]),
    ?assertMatch([[?OP_TEXT, _, #{~"d" := [_, ~"2"]}]], AB),
    ?assertMatch(
        {[[?OP_TEXT, _, _]], _, _},
        nodiff_diff(switch, B0#{kind => c}, S1, V1, [kind])
    ).

%% Two clauses calling the same helper produce two az-nodiff elements, inlined or not.
nodiff_slot_helper_switch(Config) when is_list(Config) ->
    B0 = #{kind => line, v => ~"x"},
    {_, S0, V0} = nodiff_render(helper_switch, B0),
    {Ops, _, _} = nodiff_diff(helper_switch, B0#{kind => bar}, S0, V0, [kind]),
    ?assertMatch([[?OP_TEXT, _, _], [?OP_TEXT, _, _], [?OP_TEXT, _, _]], Ops).

%% The same branch chosen again keeps the az-nodiff element untouched, even when the values
%% it was rendered from changed.
nodiff_slot_same_branch_kept(Config) when is_list(Config) ->
    B0 = #{n => 1, v => ~"a"},
    {_, S0, V0} = nodiff_render(one_branch, B0),
    {Ops, S1, V1} = nodiff_diff(one_branch, B0#{n => 2, v => ~"b"}, S0, V0, [n, v]),
    ?assertEqual([], Ops),
    ?assertEqual(maps:get(d, S0), maps:get(d, S1)),
    %% The slot still leaves the az-nodiff element.
    ?assertMatch(
        {[[?OP_TEXT, _, ~"off"]], _, _},
        nodiff_diff(one_branch, B0#{n => 0}, S1, V1, [n])
    ).

%% A switch above the az-nodiff element's own slot to a wrapper with the same statics keeps
%% the az-nodiff element; an `az_key` naming the switch remounts it.
nodiff_slot_wrapper_switch(Config) when is_list(Config) ->
    B0 = #{tab => a, v => ~"x"},
    {_, S0, V0} = nodiff_render(wrapper_switch, B0),
    {Ops, _, _} = nodiff_diff(wrapper_switch, B0#{tab => b}, S0, V0, [tab]),
    ?assertMatch([[?OP_TEXT, _, #{~"d" := [_, ~" az-key=\"b\"", ~"x"]}]], Ops).

%% A value read only inside the az-nodiff element is not a dependency of its slot.
nodiff_slot_inner_change(Config) when is_list(Config) ->
    B0 = #{n => 1, v => ~"a"},
    {_, S0, V0} = nodiff_render(one_branch, B0),
    #{deps := [SlotDeps]} = S0,
    ?assertNot(is_map_key(v, SlotDeps)),
    {Ops, S1, _} = nodiff_diff(one_branch, B0#{v => ~"b"}, S0, V0, [v]),
    ?assertEqual([], Ops),
    ?assertEqual(S0, S1).

%% An `az_key` on the az-nodiff element's root remounts it when its value changes; without one
%% the az-nodiff element stays.
nodiff_slot_az_key(Config) when is_list(Config) ->
    B0 = #{on => true, type => ~"line"},
    {_, S0, V0} = nodiff_render(keyed, B0),
    #{d := [{Az0, _}, {_Az1, _}, {Az2, _}]} = S0,
    {Ops, S1, V1} = nodiff_diff(keyed, B0#{type => ~"bar"}, S0, V0, [type]),
    ?assertEqual([Az0, Az2], lists:sort([Az || [?OP_TEXT, Az, _] <- Ops])),
    ?assertEqual(2, length(Ops)),
    ?assertMatch({[], _, _}, nodiff_diff(keyed, B0#{type => ~"bar"}, S1, V1, [type])).

%% Every spelling of the `az_key` attribute names the az-nodiff element.
nodiff_slot_az_key_spellings(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_az_key_spellings). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        case arizona_template:get(n, Bindings) of "
        "            1 -> {'p', ['az-nodiff', "
        "                {'az-key', arizona_template:get(k, Bindings)}], []}; "
        "            2 -> {'p', ['az-nodiff', "
        "                {<<\"AZ-KEY\">>, arizona_template:get(k, Bindings)}], []} "
        "        end"
        "    ]}). "
    ),
    [
        begin
            {_, S0, V0} = arizona_render:render(Mod:render(#{n => N, k => 1}), #{}),
            ?assertMatch(
                {[[?OP_TEXT, _, _]], _, _},
                arizona_diff:diff(Mod:render(#{n => N, k => 2}), S0, V0, #{k => true})
            )
        end
     || N <- [1, 2]
    ].

%% A child view inside an az-nodiff element gets its props at mount only, wherever the
%% element is evaluated from.
nodiff_slot_child_views_not_updated(Config) when is_list(Config) ->
    Item = fun(N) -> #{id => ~"in_item", n => N} end,
    B0 = #{on => true, n => 1, items => [Item(1)]},
    {_, S0, V0} = nodiff_render(child_paths, B0),
    Ids = [~"in_branch", ~"in_stateless", ~"in_item"],
    [?assertMatch(#{count := 1}, maps:get(bindings, maps:get(Id, V0))) || Id <- Ids],
    {Ops, _, V1} = nodiff_diff(child_paths, B0#{n => 2, items => [Item(2)]}, S0, V0, [n, items]),
    [?assertMatch(#{count := 1}, maps:get(bindings, maps:get(Id, V1))) || Id <- Ids],
    ?assertEqual([], Ops).

%% A child view whose own root is az-nodiff keeps its render under new props, and
%% is replaced when its id changes.
nodiff_slot_root_child_id(Config) when is_list(Config) ->
    B0 = #{cid => ~"rc1", n => 1},
    {_, S0, V0} = nodiff_render(root_child, B0),
    ?assertMatch({[], _, _}, nodiff_diff(root_child, B0#{n => 2}, S0, V0, [n])),
    {Ops, _, V1} = nodiff_diff(root_child, B0#{cid => ~"rc2"}, S0, V0, [cid]),
    ?assertMatch([[?OP_TEXT, _, #{~"s" := _}]], Ops),
    ?assertEqual([~"rc2"], maps:keys(V1)).

%% Az-nodiff elements in plain-list items come and go with their items; a value change in a
%% kept item leaves its az-nodiff element alone.
nodiff_slot_list_items(Config) when is_list(Config) ->
    {_, S0, V0} = nodiff_render(list_widgets, #{items => [#{v => ~"a"}]}),
    {Add, S1, V1} = nodiff_diff(list_widgets, #{items => [#{v => ~"a"}, #{v => ~"b"}]}, S0, V0, [
        items
    ]),
    ?assertMatch([[?OP_LIST_PATCH, _, [[?OP_INSERT | _]]]], Add),
    ?assertMatch(
        {[], _, _},
        nodiff_diff(list_widgets, #{items => [#{v => ~"z"}, #{v => ~"b"}]}, S1, V1, [items])
    ),
    {Remove, _, _} = nodiff_diff(list_widgets, #{items => [#{v => ~"a"}]}, S1, V1, [items]),
    ?assertMatch([[?OP_LIST_PATCH, _, [[?OP_REMOVE | _]]]], Remove),
    %% A new `az_key` in a kept item remounts its az-nodiff element.
    {Rekey, _, _} =
        nodiff_diff(list_widgets, #{items => [#{v => ~"a", rev => 1}, #{v => ~"b"}]}, S1, V1, [
            items
        ]),
    ?assertMatch(
        [[?OP_LIST_PATCH, _, [[?OP_ITEM_PATCH, 0, [[?OP_TEXT, _, #{~"d" := [_, _, ~"a"]}]]]]]],
        Rekey
    ).

%% Az-nodiff elements in stream items come and go with their items; updating a kept item
%% leaves its az-nodiff element alone.
nodiff_slot_stream_items(Config) when is_list(Config) ->
    KeyFun = fun(#{id := Id}) -> Id end,
    St0 = settled_stream(arizona_stream:new(KeyFun, [#{id => ~"1", v => ~"a"}])),
    {_, S0, V0} = nodiff_render(stream_widgets, #{items => St0}),
    St1 = arizona_stream:insert(St0, #{id => ~"2", v => ~"b"}),
    {Add, S1, V1} = nodiff_diff(stream_widgets, #{items => St1}, S0, V0, [items]),
    ?assertMatch([[?OP_INSERT, _, ~"2", _, _]], Add),
    St2 = settled_stream(St1),
    St3 = arizona_stream:update(St2, ~"1", #{id => ~"1", v => ~"z"}),
    {Update, S2, V2} = nodiff_diff(stream_widgets, #{items => St3}, S1, V1, [items]),
    ?assertEqual([], Update),
    St4 = arizona_stream:delete(settled_stream(St3), ~"2"),
    ?assertMatch(
        {[[?OP_REMOVE, _, ~"2"]], _, _},
        nodiff_diff(stream_widgets, #{items => St4}, S2, V2, [items])
    ),
    %% A new `az_key` in a kept item remounts its az-nodiff element.
    St5 = arizona_stream:update(settled_stream(St3), ~"1", #{id => ~"1", v => ~"z", rev => 1}),
    {Rekey, _, _} = nodiff_diff(stream_widgets, #{items => St5}, S2, V2, [items]),
    ?assertMatch([[?OP_ITEM_PATCH, _, ~"1", [[?OP_TEXT, _, #{~"d" := [_, _, ~"z"]}]]]], Rekey).

%% An az-nodiff element's root names its identity -- the fingerprint, the branch that produced
%% it and its `az_key` -- in `az-nodiff`, for the client to keep it by across a
%% wholesale re-render. The value is the one nodiff_token/3 builds from the very
%% fields same_nodiff/3 compares, however the branch got stamped: on a literal
%% template, by `branch/2` on a helper's runtime value, or through a `?stateless`
%% descriptor.
nodiff_token_names_identity(Config) when is_list(Config) ->
    {_, Keyed, _} = nodiff_render(keyed, #{on => true, type => ~"line"}),
    {_, Helpers, _} = nodiff_render(helper_switch, #{kind => bar, v => ~"x"}),
    Marked = nodiff_snaps(Keyed) ++ nodiff_snaps(Helpers),
    ?assertEqual(6, length(Marked)),
    [
        ?assertEqual(
            nodiff_token_of(Snap), nodiff_attr_of(Snap), {Snap, nodiff_token_of(Snap)}
        )
     || Snap <- Marked
    ],
    %% The branch is named on every clause path, and the key whenever there is one.
    [<<_/binary>> = Line | _] = [nodiff_attr_of(S) || S <- nodiff_snaps(Keyed)],
    ?assertMatch({_, _}, binary:match(Line, ~".b1.k")),
    [?assertMatch({_, _}, binary:match(nodiff_attr_of(S), ~".b2")) || S <- nodiff_snaps(Helpers)].

%% Two renders name the same az-nodiff element exactly when the diff keeps it: the same
%% branch with new values keeps the name, another branch calling the same helper
%% or a new `az_key` changes it.
nodiff_token_follows_same_nodiff(Config) when is_list(Config) ->
    Attr = fun(Fun, B) ->
        {_, Snap, _} = nodiff_render(Fun, B),
        [nodiff_attr_of(S) || S <- nodiff_snaps(Snap)]
    end,
    ?assertEqual(
        Attr(one_branch, #{n => 1, v => ~"a"}), Attr(one_branch, #{n => 2, v => ~"b"})
    ),
    [Line1, Line2, Line3] = Attr(helper_switch, #{kind => line, v => ~"x"}),
    [Bar1, Bar2, Bar3] = Attr(helper_switch, #{kind => bar, v => ~"x"}),
    [?assertNotEqual(L, B) || {L, B} <- [{Line1, Bar1}, {Line2, Bar2}, {Line3, Bar3}]],
    [K1, _, _] = Attr(keyed, #{on => true, type => ~"line"}),
    [K2, _, _] = Attr(keyed, #{on => true, type => ~"bar"}),
    ?assertNotEqual(K1, K2).

%% An `?each` item whose root is an az-nodiff element is named too, with the item's own
%% `az_key`.
nodiff_token_item_key(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_token_item). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'ul', [], ["
        "        arizona_template:each(fun(I) -> "
        "            {'li', ['az-nodiff', {az_key, I}], [I]} "
        "        end, arizona_template:get(items, Bindings))"
        "    ]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [1, 2]}))),
    {match, Names} = re:run(HTML, ~"<li az-nodiff=\"([^\"]+)\"", [
        global, {capture, all_but_first, binary}
    ]),
    [[A], [B]] = Names,
    [F, _] = binary:split(A, ~"."),
    ?assertEqual(arizona_template:nodiff_token(F, undefined, 1), A),
    ?assertEqual(arizona_template:nodiff_token(F, undefined, 2), B).

%% An unkeyed `?each` item is not named, bare or through `?html`: every item
%% shares the one template, so a name would not say which item it is. Its root
%% keeps the directive bare -- client-owned, never carried over.
nodiff_token_unkeyed_item(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_token_unkeyed). "
        "-export([bare/1, wrapped/1]). "
        "bare(Bindings) -> "
        "    arizona_template:html({'ul', [], ["
        "        arizona_template:each(fun(I) -> {'li', ['az-nodiff'], [I]} end, "
        "            arizona_template:get(items, Bindings))"
        "    ]}). "
        "wrapped(Bindings) -> "
        "    arizona_template:html({'ul', [], ["
        "        arizona_template:each(fun(I) -> "
        "            arizona_template:html({'li', ['az-nodiff'], [I]}) "
        "        end, arizona_template:get(items, Bindings))"
        "    ]}). "
    ),
    [
        ?assertEqual(
            ~"<ul><li az-nodiff>1</li><li az-nodiff>2</li></ul>",
            strip_markers(
                iolist_to_binary(arizona_render:render_to_iolist(Mod:F(#{items => [1, 2]})))
            )
        )
     || F <- [bare, wrapped]
    ].

strip_markers(HTML) ->
    re:replace(HTML, ~"<!--/?az[^>]*-->| az=\"[^\"]*\"", ~"", [global, {return, binary}]).

%% Only an az-nodiff element's own root is named: not the templates nested in it (they are
%% part of it), not a layout, and not a target whose client keeps no az-nodiff elements.
nodiff_token_only_on_nodiff_roots(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_nodiff_token_only). "
        "-export([marked/1, layout/1, native/1, fragment/1]). "
        "marked(Bindings) -> "
        "    arizona_template:html({'div', ['az-nodiff'], ["
        "        case arizona_template:get(on, Bindings) of "
        "            true -> {'p', [], [<<\"in\">>]}; "
        "            false -> <<>> "
        "        end, "
        "        arizona_template:html({'span', [], [<<\"nested\">>]})"
        "    ]}). "
        "layout(Bindings) -> "
        "    arizona_template:html({'main', ['az-nodiff'], [az:inner_content(Bindings)]}). "
        "native(_Bindings) -> "
        "    arizona_template:native({'Column', ['az-nodiff'], [<<\"n\">>]}). "
        "fragment(_Bindings) -> "
        "    arizona_template:html([{'p', ['az-nodiff'], [<<\"a\">>]}, {'p', [], [<<\"b\">>]}]). "
    ),
    Marked = iolist_to_binary(arizona_render:render_to_iolist(Mod:marked(#{on => true}))),
    ?assertMatch([_], binary:matches(Marked, ~"az-nodiff")),
    ?assertMatch({0, _}, binary:match(Marked, ~"<div az-nodiff=")),
    Layout = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:layout(#{inner_content => ~"page"}))
    ),
    ?assertEqual(nomatch, binary:match(Layout, ~"az-nodiff")),
    Native = iolist_to_binary(arizona_render:render_to_iolist(Mod:native(#{}))),
    ?assertEqual(nomatch, binary:match(Native, ~"az-nodiff")),
    %% A root of a multi-element fragment keeps its directive bare: the element the
    %% author marked, no other, and no token -- it is not a single az-nodiff element.
    ?assertEqual(
        ~"<p az-nodiff>a</p><p>b</p>",
        iolist_to_binary(arizona_render:render_to_iolist(Mod:fragment(#{})))
    ).

%% `az-nodiff` is a bare directive: its value in the HTML is the compiler's, so an
%% authored one, in any spelling and on any element, is a compile error.
nodiff_value_raises(Config) when is_list(Config) ->
    [
        assert_parse_error(
            lists:flatten([
                "-module(pt_nodiff_value_",
                integer_to_list(N),
                "). -export([render/1]). render(_Bindings) -> ",
                "arizona_template:html(",
                Elem,
                "). "
            ]),
            fun
                (nodiff_value) -> true;
                (_) -> false
            end
        )
     || {N, Elem} <- [
            {1, "{'div', [{'az-nodiff', <<\"x\">>}], []}"},
            {2, "{'div', [{az_nodiff, true}], []}"},
            {3, "{'div', [{<<\"AZ-NODIFF\">>, <<\"x\">>}], []}"},
            {4, "{'div', [], [{'p', [{az_nodiff, <<\"x\">>}], []}]}"}
        ]
    ].

%% Every az-nodiff element snapshot under `Snap`, in render order.
nodiff_snaps(#{d := D}) ->
    lists:append([nodiff_snaps_val(V) || {_Az, V} <- D]).

nodiff_snaps_val({arizona_esc, V}) -> nodiff_snaps_val(V);
nodiff_snaps_val(#{diff := false} = Snap) -> [Snap];
nodiff_snaps_val(#{s := _, d := _} = Snap) -> nodiff_snaps(Snap);
nodiff_snaps_val(_V) -> [].

nodiff_attr_of(#{d := [{undefined, {attr, ~"az-nodiff", Name}} | _]}) ->
    Name.

%% The token same_nodiff/3's identity spells: the unscoped fingerprint (the scope
%% prefix is the slot, which the client reads off the DOM), the branch and key.
nodiff_token_of(#{f := ScopedF} = Snap) ->
    {Branch, Key} = maps:get(nodiff, Snap, {undefined, undefined}),
    [F | _] = lists:reverse(binary:split(ScopedF, ~"-", [global])),
    arizona_template:nodiff_token(F, Branch, Key).

nodiff_render(Fun, Bindings) ->
    arizona_render:render(arizona_nodiff_slots:Fun(Bindings), #{}).

nodiff_diff(Fun, Bindings, Snap, Views, Changed) ->
    arizona_diff:diff(
        arizona_nodiff_slots:Fun(Bindings), Snap, Views, maps:from_keys(Changed, true)
    ).

settled_stream(Stream) ->
    #{s := Settled} = arizona_stream:clear_stream_pending(#{s => Stream}, [s]),
    Settled.

%% A content child that is a list mixing static text with an `?each` renders the
%% each as markup and diffs it, as a literal mixed fragment there would.
list_child_with_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({ul, [], [[<<\"x\">>, "
        "        arizona_template:each(fun(I) -> {li, [], [I]} end, "
        "            arizona_template:get(items, Bindings))]]}). "
    ),
    assert_list_child(Mod, #{items => [~"1", ~"2"]}, #{items => [~"1", ~"3"]}, [~"1", ~"2"], ~"3").

%% The same with a `?stateless` component in the list.
list_child_with_stateless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_stateless). "
        "-export([render/1, item/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], [[<<\"x\">>, "
        "        arizona_template:stateless(fun item/1, "
        "            #{y => arizona_template:get(y, Bindings)})]]}). "
        "item(Bindings) -> "
        "    arizona_template:html({i, [], [arizona_template:get(y, Bindings)]}). "
    ),
    assert_list_child(Mod, #{y => ~"Y"}, #{y => ~"Z"}, [~"Y"], ~"Z").

%% The same with a nested `?html` in the list.
list_child_with_nested_html(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], [[<<\"x\">>, "
        "        arizona_template:html({b, [], [arizona_template:get(y, Bindings)]})]]}). "
    ),
    assert_list_child(Mod, #{y => ~"Y"}, #{y => ~"Z"}, [~"Y"], ~"Z").

%% SSR equals the live render and shows the static text and every value; a change
%% to the read binding diffs to ops carrying the new value.
assert_list_child(Mod, B0, B1, Shown, New) ->
    SSR = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(B0))),
    {Live, Snap, _Views} = arizona_render:render(Mod:render(B0), #{}),
    ?assertEqual(SSR, iolist_to_binary(Live)),
    ?assertNotEqual(nomatch, binary:match(SSR, ~"x")),
    [?assertNotEqual(nomatch, binary:match(SSR, V)) || V <- Shown],
    Changed = maps:from_keys(maps:keys(B1), true),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(Mod:render(B1), Snap, #{}, Changed),
    ?assertMatch([_ | _], Ops),
    ?assertNotEqual(nomatch, binary:match(term_to_binary(Ops), New)).

%% A map literal with a non-atom key in a content-slot list is data, not a
%% template: the transform must not crash classifying it.
list_child_binary_key_map(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_bin_map). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], ["
        "        case arizona_template:get(f, Bindings) of"
        "            true -> [<<\"x\">>, #{<<\"a\">> => 1}];"
        "            false -> <<>>"
        "        end"
        "    ]}). "
    ),
    ?assertMatch(
        <<"<p", _/binary>>,
        iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{f => false})))
    ).

%% A user map that merely spells out `s`/`d`/`f` is not taken for a compiled
%% template: it stays a value (and, being unrenderable, fails like any bad value).
list_child_data_map_not_template(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_data_map). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], ["
        "        case arizona_template:get(f, Bindings) of"
        "            true -> [<<\"x\">>, #{s => [<<\"a\">>], d => [], f => <<\"z\">>}];"
        "            false -> <<>>"
        "        end"
        "    ]}). "
    ),
    ?assertError(_, arizona_render:render_to_iolist(Mod:render(#{f => true}))).

%% A guarded helper (single or multi-clause) whose body is markup only through a
%% constructor is not rejected when it is called outside a content slot: it renders
%% fine there.
list_child_guarded_helper_not_rejected(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_guarded_helper). "
        "-export([render/1, card/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], ["
        "        integer_to_binary(length(kids(arizona_template:get(r, Bindings)))),"
        "        integer_to_binary(length(kid(arizona_template:get(r, Bindings))))"
        "    ]}). "
        "kids(R) when R =:= admin -> "
        "    [<<\"a\">>, arizona_template:stateless(fun card/1, #{})]; "
        "kids(_) -> []. "
        "kid(R) when is_atom(R) -> "
        "    [<<\"b\">>, arizona_template:stateless(fun card/1, #{})]. "
        "card(_Bindings) -> arizona_template:html({i, [], [<<\"c\">>]}). "
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{r => admin}))),
            ~"2<!--/az--><!--az:"
        )
    ).

%% A hoisted `?html` reaches the list wrapped in its track block; it is still markup.
list_child_hoisted_html(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_hoisted). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    H = arizona_template:html({b, [], [arizona_template:get(y, Bindings)]}), "
        "    arizona_template:html({p, [], [[<<\"x\">>, H]]}). "
    ),
    assert_list_child(Mod, #{y => ~"Y"}, #{y => ~"Z"}, [~"Y"], ~"Z").

%% A local helper whose body is a list holding a raw `?html` is inlined like one
%% holding a component.
list_child_helper_html(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_helper_html). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], [kids(arizona_template:get(y, Bindings))]}). "
        "kids(Y) -> [<<\"x\">>, arizona_template:html({b, [], [Y]})]. "
    ),
    assert_list_child(Mod, #{y => ~"Y"}, #{y => ~"Z"}, [~"Y"], ~"Z").

%% The same with a raw `?each` in the helper body.
list_child_helper_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_helper_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({ul, [], [kids(arizona_template:get(items, Bindings))]}). "
        "kids(Is) -> [<<\"x\">>, arizona_template:each(fun(I) -> {li, [], [I]} end, Is)]. "
    ),
    assert_list_child(Mod, #{items => [~"1", ~"2"]}, #{items => [~"1", ~"3"]}, [~"1", ~"2"], ~"3").

%% An `?each` handed to a local element helper is compiled before the helper is
%% inlined, so the helper's body is marked around an already-compiled pairing. That
%% pairing must pass through the marking untouched under every target.
helper_arg_each_native(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_helper_arg_each_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native({column, [], [wrap(arizona_template:each("
        "        fun(I) -> {text, [], [I]} end, arizona_template:get(items, Bindings)))]}). "
        "wrap(X) -> {box, [], [X]}. "
    ),
    Wire = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [~"a", ~"b"]}))),
    ?assertMatch(
        #{
            ~"type" := ~"column",
            ~"children" := [
                #{
                    ~"type" := ~"box",
                    ~"children" := [
                        #{
                            ~"children" := [
                                [#{~"type" := ~"text"}, #{~"type" := ~"text"}]
                            ]
                        }
                    ]
                }
            ]
        },
        json:decode(Wire)
    ).

helper_arg_each_terminal(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_helper_arg_each_terminal). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal({col, [], [wrap(arizona_template:each("
        "        fun(I) -> {line, [], [I]} end, arizona_template:get(items, Bindings)))]}). "
        "wrap(X) -> {col, [], [X]}. "
    ),
    Out = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [~"a", ~"b"]}))),
    ?assertEqual(nomatch, binary:match(Out, [~"<", ~"{"])),
    ?assertNotEqual(nomatch, binary:match(Out, ~"a")),
    ?assertNotEqual(nomatch, binary:match(Out, ~"b")).

%% Under `?html` the pairing passes through a helper that opens `<svg>`.
helper_arg_each_svg(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_helper_arg_each_svg). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({main, [], [wrap(arizona_template:each("
        "        fun(I) -> {title, [], [I]} end, arizona_template:get(items, Bindings)))]}). "
        "wrap(X) -> {svg, [], [X]}. "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [~"a", ~"b"]}))),
    ?assertMatch({match, _}, re:run(HTML, ~"<main><svg[^>]*><!--[^>]*--><title")),
    ?assertMatch(
        {match, [[~"a"], [~"b"]]},
        re:run(HTML, ~"<title[^>]*>(?:<!--[^>]*-->)?([ab])", [
            global, {capture, all_but_first, binary}
        ])
    ).

%% A hoisted `?each` is bound outside every template, so nothing at its binding
%% says what it renders in. It compiles where its variable is used, exactly as if
%% it were written there: under `?native` its items are native JSON nodes.
hoisted_each_native(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_hoisted_each_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Items = arizona_template:each("
        "        fun(I) -> {text, [], [I]} end, arizona_template:get(items, Bindings)), "
        "    arizona_template:native({column, [], [Items]}). "
    ),
    Wire = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [~"a", ~"b"]}))),
    ?assertMatch(
        #{
            ~"type" := ~"column",
            ~"children" := [
                #{
                    ~"children" := [
                        [
                            #{~"type" := ~"text", ~"children" := [#{~"children" := [~"a"]}]},
                            #{~"type" := ~"text", ~"children" := [#{~"children" := [~"b"]}]}
                        ]
                    ]
                }
            ]
        },
        json:decode(Wire)
    ).

hoisted_each_terminal(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_hoisted_each_terminal). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Items = arizona_template:each("
        "        fun(I) -> {line, [], [I]} end, arizona_template:get(items, Bindings)), "
        "    arizona_template:terminal({col, [], [Items]}). "
    ),
    Out = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{items => [~"a", ~"b"]}))),
    ?assertEqual(nomatch, binary:match(Out, [~"<", ~"{"])),
    ?assertMatch(
        {match, [[~"a"], [~"b"]]},
        re:run(Out, ~"([ab])", [global, {capture, all_but_first, binary}])
    ).

%% Used directly inside `<svg>`, a hoisted `?each` compiles its items in foreign
%% content, where `<title>` is ordinary markup: each item's slot keeps its markers
%% and an item change patches that slot alone.
hoisted_each_svg(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_hoisted_each_svg). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Items = arizona_template:each("
        "        fun(I) -> {title, [], [I]} end, arizona_template:get(items, Bindings)), "
        "    arizona_template:html({svg, [], [Items]}). "
    ),
    assert_svg_title_items(Mod).

%% An `?each` keeps the content context of where it is written -- or, hoisted,
%% where its variable is used. A local element helper's body is not that place: an
%% `?each` passed as a helper argument is in the caller's context, here HTML, even
%% though the helper wraps it in `<svg>`. So its `<title>` items are escapable raw
%% text (markerless) and an item change re-renders the item's title whole.
hoisted_each_helper_arg_svg(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_hoisted_each_helper_svg). "
        "-export([hoisted/1, inline/1]). "
        "hoisted(Bindings) -> "
        "    Items = arizona_template:each("
        "        fun(I) -> {title, [], [I]} end, arizona_template:get(items, Bindings)), "
        "    arizona_template:html({main, [], [wrap(Items)]}). "
        "inline(Bindings) -> "
        "    arizona_template:html({main, [], [wrap(arizona_template:each("
        "        fun(I) -> {title, [], [I]} end, arizona_template:get(items, Bindings)))]}). "
        "wrap(X) -> {svg, [], [X]}. "
    ),
    lists:foreach(
        fun(F) ->
            {HTML, _} = arizona_render:render(Mod:F(#{items => [~"a", ~"b"]})),
            ?assertMatch(
                {match, [[~"a"], [~"b"]]},
                re:run(HTML, ~"<title az=\"[^\"]+\">([ab])</title>", [
                    global, {capture, all_but_first, binary}
                ])
            )
        end,
        [hoisted, inline]
    ).

%% Marking the splice site checks a hoisted template like a written one: a
%% hoisted `?html` used inside `?native` is cross-target nesting at compile time,
%% not a render-time `cross_target_child`.
hoisted_cross_target_nesting(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_hoisted_cross_target). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Row = arizona_template:html({p, [], [arizona_template:get(x, Bindings)]}), "
        "    arizona_template:native({column, [], [Row]}). ",
        fun
            (cross_target_nesting) -> true;
            (_) -> false
        end
    ).

assert_svg_title_items(Mod) ->
    {HTML0, Snap0, Views0} = arizona_render:render(Mod:render(#{items => [~"a", ~"b"]}), #{}),
    ?assertMatch(
        {match, [[~"a"], [~"b"]]},
        re:run(HTML0, ~"<title az=\"[^\"]+\"><!--az:[^>]+-->([ab])<!--/az--></title>", [
            global, {capture, all_but_first, binary}
        ])
    ),
    {Ops, _, _} =
        arizona_diff:diff(Mod:render(#{items => [~"a", ~"c"]}), Snap0, Views0, #{items => true}),
    ?assertMatch([[?OP_LIST_PATCH, _, [[?OP_ITEM_PATCH, 1, [[?OP_TEXT, _, ~"c"]]]]]], Ops).

%% `?inner_content` beside static text in a layout's list gets its own slot.
list_child_inner_content(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_inner). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({main, [], [[<<\"x\">>, az:inner_content(Bindings)]]}). "
    ),
    HTML = iolist_to_binary(
        arizona_render:render_to_iolist(Mod:render(#{inner_content => #{s => [~"PAGE"], d => []}}))
    ),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"x")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"PAGE")).

%% A whole-value `?raw` beside a plain value is spliced verbatim, the value escaped.
list_child_raw_beside_value(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_raw). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], [[arizona_template:raw(<<\"<i>r</i>\">>), "
        "        arizona_template:get(y, Bindings)]]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{y => ~"<y>"}))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<i>r</i>")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"&lt;y&gt;")).

%% A `?local` in a content-slot list has no element to carry its descriptor.
list_child_local_orphaned(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_list_child_local). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], [[<<\"x\">>, arizona_template:local(k, 0)]]}). ",
        fun(R) -> R =:= local_orphaned end
    ).

%% A content list as a conditional tail.
list_child_case_tail(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_case). "
        "-export([render/1, item/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], ["
        "        case arizona_template:get(f, Bindings) of"
        "            true -> [<<\"x\">>, arizona_template:stateless(fun item/1, "
        "                #{y => arizona_template:get(y, Bindings)})];"
        "            false -> <<>>"
        "        end"
        "    ]}). "
        "item(Bindings) -> arizona_template:html({i, [], [arizona_template:get(y, Bindings)]}). "
    ),
    assert_list_child(Mod, #{f => true, y => ~"Y"}, #{f => true, y => ~"Z"}, [~"Y"], ~"Z").

%% A `?stateful` child in a content list mounts and renders its own view.
list_child_stateful(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_stateful). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({section, [], [[<<\"x\">>, "
        "        arizona_template:stateful(arizona_counter, "
        "            #{id => <<\"lc\">>, count => arizona_template:get(n, Bindings)})]]}). "
    ),
    {HTML, _Snap, Views} = arizona_render:render(Mod:render(#{n => 7}), #{}),
    ?assertMatch(#{~"lc" := _}, Views),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), ~"x")),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), ~"7")).

%% The terminal target renders a content list holding a nested template.
list_child_terminal(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_term). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal({line, [], [[<<\"x\">>, "
        "        arizona_template:terminal({text, [], [arizona_template:get(y, Bindings)]})]]}). "
    ),
    Out = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{y => ~"Y"}))),
    ?assertNotEqual(nomatch, binary:match(Out, ~"x")),
    ?assertNotEqual(nomatch, binary:match(Out, ~"Y")).

%% Under az-nodiff the list renders once, markerless.
list_child_nodiff(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({ul, ['az-nodiff'], [[<<\"x\">>, "
        "        arizona_template:each(fun(I) -> {li, [], [I]} end, "
        "            arizona_template:get(items, Bindings))]]}). "
    ),
    HTML = render_markerless(Mod:render(#{items => [~"1", ~"2"]})),
    ?assertEqual(~"<ul>x<li>1</li><li>2</li></ul>", HTML).

%% A `?html`-wrapped helper is a template-returning call. Off a content tail (a
%% function argument, a case scrutinee) the call stays, so the consumer gets the
%% template it returned rather than the unwrapped list.
list_child_wrapped_helper_off_tail(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_off_tail). "
        "-export([render/1, inner/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], ["
        "        element(1, {lst(arizona_template:get(a, Bindings))})]}). "
        "lst(Y) -> arizona_template:html([<<\"x\">>, "
        "    arizona_template:stateless(fun inner/1, #{a => Y})]). "
        "inner(Bindings) -> arizona_template:html({i, [], [arizona_template:get(a, Bindings)]}). "
    ),
    assert_list_child(Mod, #{a => ~"A"}, #{a => ~"B"}, [~"A"], ~"B").

%% A wrapped helper in a try/receive/maybe tail is inlined like a case tail, so its
%% reads stay tracked on the slot.
wrapped_helper_try_tail_tracked(Config) when is_list(Config) ->
    assert_tail_tracked(
        "pt_wrapped_try",
        "try w(Bindings) catch _:_ -> <<>> end"
    ).

wrapped_helper_receive_tail_tracked(Config) when is_list(Config) ->
    assert_tail_tracked(
        "pt_wrapped_receive",
        "receive never -> <<>> after 0 -> w(Bindings) end"
    ).

wrapped_helper_maybe_tail_tracked(Config) when is_list(Config) ->
    assert_tail_tracked(
        "pt_wrapped_maybe",
        "maybe ok ?= ok, w(Bindings) end"
    ).

assert_tail_tracked(Name, Tail) ->
    Mod = compile_module(
        "-module(" ++ Name ++
            "). "
            "-export([render/1]). "
            "render(Bindings) -> arizona_template:html({p, [], [" ++ Tail ++
            "]}). "
            "w(Bindings) -> arizona_template:html({b, [], [arizona_template:get(x, Bindings)]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{x => ~"X0"}), #{}),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(Mod:render(#{x => ~"X1"}), Snap, #{}, #{x => true}),
    ?assertNotEqual(nomatch, binary:match(term_to_binary(Ops), ~"X1")).

%% An element-bodied `?html`-wrapped helper used as a value (a case scrutinee) stays
%% a call, so the consumer gets its template rather than the unwrapped element.
list_child_wrapped_element_helper_off_tail(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_off_tail_el). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], ["
        "        case card(arizona_template:get(a, Bindings)) of T -> T end"
        "    ]}). "
        "card(Y) -> arizona_template:html({b, [], [Y]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{a => ~"A"}))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<b")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"A")).

%% A stream in a content-list fragment keeps per-item ops. They target the each slot's
%% own az, which no element carries: the client resolves it through the slot's markers
%% and places the item inside their span.
list_child_stream_insert(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_stream). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({ul, [], [[<<\"x\">>, "
        "        arizona_template:each(fun(I, K) -> {li, [{az_key, K}], [I]} end, "
        "            arizona_template:get(s, Bindings))]]}). "
    ),
    S0 = arizona_stream:new(fun(X) -> X end, [~"a", ~"b"]),
    {HTML, Snap, _Views} = arizona_render:render(Mod:render(#{s => S0}), #{}),
    S1 = arizona_stream:insert(S0, ~"c"),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(Mod:render(#{s => S1}), Snap, #{}, #{s => true}),
    ?assertMatch([[?OP_INSERT, _, ~"c", -1, _]], Ops),
    [[?OP_INSERT, Target | _]] = Ops,
    ?assertEqual(nomatch, binary:match(iolist_to_binary(HTML), <<"az=\"", Target/binary, "\"">>)),
    ?assertNotEqual(
        nomatch, binary:match(iolist_to_binary(HTML), <<"<!--az:", Target/binary, "-->">>)
    ).

%% A plain-list `?each` in a content-list fragment re-renders its own slot as a
%% whole (one marker-anchored OP_TEXT), not item by item.
list_child_plain_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_plain). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({ul, [], [[<<\"x\">>, "
        "        arizona_template:each(fun(I) -> {li, [], [I]} end, "
        "            arizona_template:get(items, Bindings))]]}). "
    ),
    {_HTML, Snap, _Views} = arizona_render:render(Mod:render(#{items => [~"a", ~"b"]}), #{}),
    {Ops, _Snap1, _Views1} = arizona_diff:diff(
        Mod:render(#{items => [~"a", ~"z"]}), Snap, #{}, #{items => true}
    ),
    ?assertMatch([[?OP_TEXT, _, #{~"t" := _}]], Ops).

%% A conditional item whose tail is markup makes the list a fragment.
list_child_case_item_markup(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_list_child_case_item). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({p, [], [[<<\"x\">>, "
        "        case arizona_template:get(f, Bindings) of"
        "            true -> {b, [], [arizona_template:get(a, Bindings)]};"
        "            false -> <<>>"
        "        end]]}). "
    ),
    assert_list_child(
        Mod, #{f => true, a => ~"A"}, #{f => true, a => ~"B"}, [~"<b", ~"A"], ~"B"
    ).

%% Escapable raw text (`<title>`, `<textarea>`) shows markup as literal text, so a
%% markup constructor in its content is refused at compile time.
raw_text_rejects_each(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_raw_text_each). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({textarea, [], ["
        "        arizona_template:each(fun(I) -> {b, [], [I]} end, "
        "            arizona_template:get(items, Bindings))]}). ",
        fun(R) -> R =:= markup_in_raw_text end
    ).

raw_text_rejects_element_child(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_raw_text_elem). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({title, [], [{b, [], [arizona_template:get(t, Bindings)]}]}). ",
        fun(R) -> R =:= markup_in_raw_text end
    ).

raw_text_rejects_inlined_element_helper(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_raw_text_helper). "
        "-export([render/1]). "
        "render(_Bindings) -> arizona_template:html({title, [], [h()]}). "
        "h() -> {b, [], [<<\"x\">>]}. ",
        fun(R) -> R =:= markup_in_raw_text end
    ).

%% A nested template with no dynamics and no tag renders as plain text.
raw_text_keeps_text_template(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_raw_text_text_tmpl). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({title, [], [arizona_template:html(<<\"x\">>)]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{}))),
    ?assertMatch({match, _}, re:run(HTML, ~"^<title[^>]*>x</title>$")).

raw_text_rejects_branch_markup(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_raw_text_branch). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({title, [], ["
        "        case arizona_template:get(f, Bindings) of"
        "            true -> arizona_template:html({b, [], [<<\"x\">>]});"
        "            false -> <<\"plain\">>"
        "        end]}). ",
        fun(R) -> R =:= markup_in_raw_text end
    ).

%% A value, a `?raw` and a value branch stay allowed there.
raw_text_keeps_values(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_raw_text_values). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({textarea, [], ["
        "        case arizona_template:get(f, Bindings) of"
        "            true -> arizona_template:get(y, Bindings);"
        "            false -> arizona_template:raw(<<\"&amp;\">>)"
        "        end]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{f => true, y => ~"<y>"}))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"&lt;y&gt;")).

%% A ?raw in a title list is a value, as a bare ?raw there is: spliced as text.
raw_text_list_with_raw_is_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_raw_text_raw_list). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({title, [], [[<<\"a \">>, "
        "        arizona_template:raw(arizona_template:get(r, Bindings))]]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{r => ~"&amp;"}))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"a &amp;</title>")),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--")).

%% A content-slot list item that is a variable bound to markup is a compile error
%% naming the variable: only markup written in the list is compiled.
markup_var_in_content_list_rejected(Config) when is_list(Config) ->
    [
        assert_parse_error(
            "-module(pt_markup_var_" ++ Suffix ++
                "). "
                "-export([render/1]). "
                "render(Bindings) -> " ++ Body,
            fun(R) -> R =:= {markup_var_in_content_list, 'Hdr'} end
        )
     || {Suffix, Body} <- [
            {"each",
                "Hdr = arizona_template:html({h1, [], [<<\"T\">>]}), "
                "arizona_template:html({ul, [], [[Hdr, arizona_template:each("
                "    fun(I) -> {li, [], [I]} end, arizona_template:get(xs, Bindings))]]}). "},
            {"value",
                "Hdr = {h1, [], [<<\"T\">>]}, "
                "arizona_template:html({p, [], [[<<\"x\">>, Hdr, "
                "    arizona_template:get(a, Bindings)]]}). "},
            {"tail",
                "Hdr = [<<\"x\">>, {b, [], []}], "
                "arizona_template:html({p, [], [case arizona_template:get(f, Bindings) of "
                "    true -> [Hdr, <<\"y\">>]; false -> <<>> end]}). "}
        ]
    ],
    ok.

%% A hoisted template that reads a binding is substituted back, so it is markup
%% written in the list; and a fun parameter or generator of the same name shadows a
%% markup variable.
markup_var_in_content_list_allowed(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_markup_var_ok). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    Hdr = arizona_template:html({h1, [], [arizona_template:get(t, Bindings)]}), "
        "    Row = {b, [], []}, "
        "    arizona_template:html({ul, [], ["
        "        [Hdr, arizona_template:each(fun(Row) -> {li, [], [[<<\"r\">>, Row]]} end, "
        "            arizona_template:get(xs, Bindings))]]}). "
    ),
    assert_list_child(
        Mod, #{t => ~"T", xs => [~"x1"]}, #{t => ~"U", xs => [~"x1"]}, [~"T", ~"x1"], ~"U"
    ),
    ?assertEqual(
        pt_markup_var_gen,
        compile_module(
            "-module(pt_markup_var_gen). "
            "-export([render/1]). "
            "render(_Bindings) -> "
            "    Row = {b, [], []}, "
            "    [arizona_template:html({i, [], [[<<\"g\">>, Row]]}) || Row <- [<<\"x\">>]]. "
        )
    ).

%% =============================================================================
%% Literal lists fold to statics
%% =============================================================================

%% An attribute whose value is a list of literals is static: concatenated at
%% compile time, no dynamic, no `az` on an element that had no other dynamic.
literal_list_attr_folds(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_attr). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'p', [{class, [<<\"foo\">>, <<\"bar\">>]}, {id, \"str\"}], "
        "            [<<\"baz\">>, <<\"qux\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<p class=\"foobar\" id=\"str\">bazqux</p>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% The folded value is byte-identical to what the runtime renders for the same
%% list held in a binding -- nested lists, strings, char codes, UTF-8, and the
%% attribute escaping of every HTML-significant character.
literal_list_attr_matches_runtime(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_attr_rt). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'p', [{title, [<<\"<a>\">>, \"&b\", [<<\"\\\"'\">>, $c, 16#2764], []]}], []}"
        "    ). "
    ),
    Value = [<<"<a>">>, "&b", [<<"\"'">>, $c, 16#2764], []],
    T = Mod:render(#{}),
    ?assertEqual([], maps:get(d, T)),
    Runtime = arizona_html:render_attr(<<"title">>, Value),
    ?assertEqual(<<" title=\"&lt;a&gt;&amp;b&quot;&#39;c", 16#2764/utf8, "\"">>, Runtime),
    ?assertEqual([<<"<p", Runtime/binary, "></p>">>], maps:get(s, T)).

%% ?native: the folded prop is the JSON the runtime path produces.
literal_list_attr_native_matches_runtime(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_attr_native). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:native("
        "        {'Text', [{label, [<<\"a\">>, [<<\"\\\"b\">>, $c]]}], []}"
        "    ). "
    ),
    Value = [<<"a">>, [<<"\"b">>, $c]],
    T = Mod:render(#{}),
    ?assertEqual([], maps:get(d, T)),
    Prop = <<
        (arizona_native:attr_dyn_name(<<"label">>))/binary,
        (arizona_native:render_attr(<<"label">>, Value))/binary
    >>,
    ?assertEqual(
        [<<"{\"type\":\"Text\"", Prop/binary, ",\"children\":[]}">>], maps:get(s, T)
    ),
    ?assertEqual(<<"a\"bc">>, maps:get(<<"label">>, json:decode(hd(maps:get(s, T))))).

%% An empty list value is the empty string, as at runtime.
literal_list_attr_empty(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_attr_empty). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'p', [{class, []}, {id, [[], []]}], []}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([<<"<p class=\"\" id=\"\"></p>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)).

%% A nested literal list in content is a value: folded, and escaped exactly as
%% the runtime escapes it -- unlike a bare binary literal, which is raw markup.
literal_list_content_folds(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_content). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html("
        "        {'p', [], [[<<\"<a>\">>, [\"&\", $b]], <<\"<i>c</i>\">>]}"
        "    ). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([], maps:get(d, T)),
    Escaped = arizona_template:escape_value(arizona_html, [<<"<a>">>, ["&", $b]]),
    ?assertEqual(<<"&lt;a&gt;&amp;b">>, Escaped),
    ?assertEqual([<<"<p>", Escaped/binary, "<i>c</i></p>">>], maps:get(s, T)).

literal_list_content_native_matches_runtime(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_content_native). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:native({'Column', [], [<<\"x\">>, [<<\"a\">>, \"\\\"b\"]]}). "
    ),
    T = Mod:render(#{}),
    ?assertEqual([], maps:get(d, T)),
    Value = arizona_template:escape_value(arizona_native, [<<"a">>, "\"b"]),
    ?assertEqual(
        [<<"{\"type\":\"Column\",\"children\":[\"x\",", Value/binary, "]}">>],
        maps:get(s, T)
    ),
    ?assertMatch(#{<<"children">> := [<<"x">>, <<"a\"b">>]}, json:decode(hd(maps:get(s, T)))).

%% Conditional tails and ?each item templates compile through the same element
%% path, so their literal lists fold too: no `az` on the span, no slot marker
%% around the item's literal text.
literal_list_in_conditional_and_each(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_nested). "
        "-export([branch/1, each/1]). "
        "branch(Bindings) -> "
        "    arizona_template:html({'p', [], ["
        "        case arizona_template:get(on, Bindings) of "
        "            true -> {span, [{class, [<<\"a\">>, <<\"b\">>]}], [[<<\"x\">>, <<\"y\">>]]}; "
        "            false -> <<>> "
        "        end"
        "    ]}). "
        "each(Bindings) -> "
        "    arizona_template:html({ul, [], ["
        "        arizona_template:each(fun(I) -> "
        "            {li, [{class, [<<\"a\">>, <<\"b\">>]}], [[<<\"x\">>, <<\"y\">>], I]} "
        "        end, arizona_template:get(items, Bindings))"
        "    ]}). "
    ),
    CondHTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:branch(#{on => true}))),
    ?assertNotEqual(nomatch, binary:match(CondHTML, <<"--><span class=\"ab\">xy</span><!--">>)),
    EachHTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:each(#{items => [<<"i">>]}))),
    ?assertMatch(
        {match, _},
        re:run(EachHTML, <<"<li az=\"[^\"]+\" class=\"ab\">xy<!--az:[^>]+-->i<!--/az--></li>">>)
    ).

%% An element whose only dynamic was the folded attribute carries none: the
%% snapshot holds only the sibling's slot and a change to it is the only op.
literal_list_attr_diff_no_op(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_attr_diff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'div', [], ["
        "        {p, [{class, [<<\"a\">>, <<\"b\">>]}], [<<\"p\">>]}, "
        "        arizona_template:get(x, Bindings)"
        "    ]}). "
    ),
    T1 = Mod:render(#{x => <<"1">>}),
    ?assertMatch([{_, {esc, _}, _}], maps:get(d, T1)),
    {HTML, Snap} = arizona_render:render(T1),
    ?assertNotEqual(nomatch, binary:match(iolist_to_binary(HTML), <<"<p class=\"ab\">p</p>">>)),
    ?assertMatch(#{d := [_]}, Snap),
    {Same, _} = arizona_diff:diff(Mod:render(#{x => <<"1">>}), Snap),
    ?assertEqual([], Same),
    {Ops, _} = arizona_diff:diff(Mod:render(#{x => <<"2">>}), Snap),
    ?assertMatch([[?OP_TEXT, _, <<"2">>]], Ops).

%% A folded literal list takes no content slot, so a following ?local is slot 0
%% in both the descriptor and its marker.
literal_list_content_local_slot(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_local_slot). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'p', [], ["
        "        [<<\"a\">>], arizona_template:local(<<\"k\">>, <<\"v\">>)"
        "    ]}). "
    ),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(Mod:render(#{}))),
    ?assertEqual(#{~"c" => #{~"0" => ~"k"}}, decode_local_descriptor(HTML)),
    ?assertMatch(
        {match, _}, re:run(HTML, <<"<p az=\"([^\"]+)\"[^>]*>a<!--az:\\1-->v<!--/az--></p>">>)
    ).

%% A list holding anything evaluated stays a runtime value: a binding read, a
%% variable, an atom (which the runtime rejects, and still does).
literal_list_mixed_not_folded(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_mixed). "
        "-export([get/1, var/1, atom/1, content/1]). "
        "get(Bindings) -> "
        "    arizona_template:html("
        "        {'p', [{class, [<<\"a\">>, arizona_template:get(c, Bindings)]}], []}"
        "    ). "
        "var(_Bindings) -> "
        "    C = <<\"c\">>, "
        "    arizona_template:html({'p', [{class, [<<\"a\">>, C]}], []}). "
        "atom(_Bindings) -> "
        "    arizona_template:html({'p', [{class, [<<\"a\">>, b]}], []}). "
        "content(Bindings) -> "
        "    arizona_template:html({'p', [], [[<<\"a\">>, arizona_template:get(c, Bindings)]]}). "
    ),
    [
        ?assertMatch([{_, {attr, <<"class">>, _}, _}], maps:get(d, Mod:F(#{c => <<"c">>})))
     || F <- [get, var, atom]
    ],
    ?assertEqual(
        <<"<p az=\"0\" class=\"ac\"></p>">>,
        strip_fp(iolist_to_binary(arizona_render:render_to_iolist(Mod:get(#{c => <<"c">>}))))
    ),
    ?assertError(
        {arizona_loc, _, {bad_template_value, [<<"a">>, b]}},
        arizona_render:render_to_iolist(Mod:atom(#{}))
    ),
    ?assertMatch([{_, {esc, _}, _}], maps:get(d, Mod:content(#{c => <<"c">>}))).

%% `?raw` and effect commands keep their own paths. A folded command is static,
%% so an element with no other dynamic carries no `az`.
literal_list_raw_and_command_not_folded(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_fold_raw_cmd). "
        "-export([raw/1, cmd/1]). "
        "raw(_Bindings) -> "
        "    arizona_template:html("
        "        {'a', [{href, [<<\"/x?\">>, arizona_template:raw(<<\"a&b\">>)]}], []}"
        "    ). "
        "cmd(_Bindings) -> "
        "    arizona_template:html("
        "        {'button', [{az_click, [arizona_js:push_event(<<\"inc\">>)]}], []}"
        "    ). "
    ),
    RawT = Mod:raw(#{}),
    ?assertMatch([{_, {attr, <<"href">>, _}, _}], maps:get(d, RawT)),
    ?assertNotEqual(
        nomatch,
        binary:match(iolist_to_binary(arizona_render:render_to_iolist(RawT)), <<"href=\"/x?a&b\"">>)
    ),
    CmdT = Mod:cmd(#{}),
    ?assertEqual([], maps:get(d, CmdT)),
    Cmd = arizona_html:attr_command(<<"az-click">>, [arizona_js:push_event(<<"inc">>)]),
    ?assertEqual([<<"<button", Cmd/binary, "></button>">>], maps:get(s, CmdT)).

%% Raw text keeps its own slot policy: a literal list in <script> is still a
%% dynamic that must be a literal ?raw.
literal_list_script_still_rejected(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_fold_script). "
        "-export([render/1]). "
        "render(_Bindings) -> "
        "    arizona_template:html({'script', [], [[<<\"a\">>, <<\"b\">>]]}). ",
        fun(R) -> R =:= dynamic_in_raw_text end
    ).

strip_fp(HTML) ->
    re:replace(HTML, <<" az=\"[^\"-]+-">>, <<" az=\"">>, [global, {return, binary}]).

%% ============================================================================
%% Improper template lists
%% ============================================================================

%% A template's attrs, children and bodies are laid out one slot per item at
%% compile time, so a list ending in an expression has no length to lay out. It
%% is a compile error naming the tail's line, not a crash inside the transform.
improper_template_list_rejected(Config) when is_list(Config) ->
    lists:foreach(
        fun({Name, Template}) ->
            Source =
                "-module(" ++ Name ++
                    ").\n"
                    "-export([render/1]).\n"
                    "render(Bindings) ->\n"
                    "    arizona_template:html(\n" ++ Template ++
                    "    ).\n",
            ?assertMatch(
                {error, [{_File, [{6, arizona_parse_transform, improper_template_list}]}], []},
                arizona_parse_transform:parse_transform(parse_forms(Source), []),
                Name
            )
        end,
        [
            {"pt_improper_attrs",
                "{'div', [{class, arizona_template:get(c, Bindings)}\n"
                " | arizona_template:get(tail, Bindings)], [<<\"x\">>]}\n"},
            {"pt_improper_children",
                "{'ul', [], [<<\"first\">>\n | arizona_template:get(tail, Bindings)]}\n"},
            {"pt_improper_body", "[{'p', [], []}\n | arizona_template:get(tail, Bindings)]\n"},
            {"pt_improper_content_list",
                "{'p', [], [[{'span', [], []}\n | arizona_template:get(tail, Bindings)]]}\n"}
        ]
    ),
    ?assertMatch(
        [_ | _], arizona_parse_transform:format_error(improper_template_list)
    ).

%% An attribute value list is text, so an improper tail is one more part of the
%% chardata, exactly as a content value list already is -- with a literal ?raw
%% part too, which stays verbatim while the tail is escaped.
improper_attr_value_is_text(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_improper_attr_value). "
        "-export([plain/1, raw/1]). "
        "plain(Bindings) -> "
        "    arizona_template:html("
        "        {'a', [{href, [<<\"/u/\">> | arizona_template:get(id, Bindings)]}], []}"
        "    ). "
        "raw(Bindings) -> "
        "    arizona_template:html("
        "        {'a', [{href, [<<\"/u?\">>,"
        "            arizona_template:raw(arizona_template:get(q, Bindings))"
        "            | arizona_template:get(id, Bindings)]}], []}"
        "    ). "
    ),
    Render = fun(T) -> iolist_to_binary(arizona_render:render_to_iolist(T)) end,
    ?assertNotEqual(
        nomatch, binary:match(Render(Mod:plain(#{id => ~"7&"})), ~"href=\"/u/7&amp;\"")
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(Render(Mod:raw(#{q => ~"a&b", id => ~"&"})), ~"href=\"/u?a&b&amp;\"")
    ),
    ?assertMatch(
        {error, [{_File, [{_Line, arizona_parse_transform, local_attr_mixed}]}], []},
        arizona_parse_transform:parse_transform(
            parse_forms(
                "-module(pt_improper_attr_local). "
                "-export([render/1]). "
                "render(Bindings) -> "
                "    arizona_template:html("
                "        {'a', [{href, [<<\"/u/\">>, arizona_template:local(k, <<\"1\">>)"
                "            | arizona_template:get(id, Bindings)]}], []}"
                "    ). "
            ),
            []
        )
    ).
