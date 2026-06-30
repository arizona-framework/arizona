-module(arizona_parse_transform_SUITE).
-include_lib("stdlib/include/assert.hrl").
-dialyzer({nowarn_function, to_bin_crash/1}).

-export([all/0, groups/0]).
-export([
    az_each_basic/1,
    az_each_inside_html/1,
    each_with_named_fun_ref/1,
    each_with_named_fun_ref_arity_2/1,
    each_named_fun_ref_non_element_rejected/1,
    each_named_fun_ref_arity_2_non_element_rejected/1,
    each_with_remote_fun_ref/1,
    each_named_fun_ref_diffs/1,
    each_named_fun_ref_pattern_param/1,
    each_named_fun_ref_guard/1,
    each_named_fun_ref_prefix_body/1,
    each_named_fun_ref_nested_each/1,
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
    each_native_single_root_flag/1,
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
    terminal_renders_styled_ansi/1,
    terminal_custom_sgr_attr/1,
    terminal_fg_attr/1,
    terminal_unknown_style_rejected/1,
    terminal_unknown_attr_rejected/1,
    terminal_event_command_rejected/1,
    terminal_dynamic_attr_rejected/1,
    terminal_each_renders/1,
    deeply_nested/1,
    diff_integration/1,
    dynamic_only_child/1,
    empty_children/1,
    event_attrs_static/1,
    fingerprint_stability/1,
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
    invalid_element_binary_tag/1,
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
    nodiff_with_other_attrs/1,
    render_integration/1,
    shared_az/1,
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
    local_in_nodiff_error/1,
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
    local_in_each_renders/1,
    local_atom_key/1,
    local_atom_binary_reuse_error/1,
    ssr_escapes_content_value/1,
    ssr_escapes_attr_value/1,
    ssr_escape_all_unsafe_chars/1,
    raw_opt_out_not_escaped/1,
    az_raw_opt_out_not_escaped/1,
    block_value_not_escaped/1,
    value_template_inner_escaped/1,
    diff_value_stays_raw/1,
    fingerprint_escapes_value/1,
    native_backend_not_escaped/1
]).
-export([
    raw_text_script_markerless/1,
    raw_text_script_not_escaped/1,
    raw_text_style_markerless/1,
    raw_text_json_ld_raw_verbatim/1,
    raw_text_title_escaped_markerless/1,
    raw_text_textarea_markerless/1,
    raw_text_render_once_no_diff/1,
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
    cond_begin_block_tail/1,
    cond_try_bare_element/1,
    cond_maybe_bare_element/1,
    cond_receive_bare_element/1
]).

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
        {group, tracked_get},
        {group, tracked_guard},
        {group, layout},
        {group, utf8},
        {group, errors},
        {group, cross_target},
        {group, terminal},
        {group, az_macros},
        {group, az_view}
    ].

groups() ->
    [
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
            event_attrs_static,
            empty_children,
            fingerprint_stability,
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
            static_after_dynamic_attr
        ]},
        %% Client-owned slots (?local)
        {local, [parallel], [
            local_content_render,
            local_attr_render,
            local_diff_skipped,
            local_key_not_literal_error,
            local_content_multi_slot,
            local_in_nodiff_error,
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
            local_in_each_renders,
            local_atom_key,
            local_atom_binary_reuse_error
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
            each_with_remote_fun_ref,
            each_named_fun_ref_diffs,
            each_named_fun_ref_pattern_param,
            each_named_fun_ref_guard,
            each_named_fun_ref_prefix_body,
            each_named_fun_ref_nested_each,
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
            each_native_single_root_flag,
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
            bool_attr_true,
            bool_attr_false,
            bare_binary_bool,
            bool_dynamic_true,
            bool_dynamic_false,
            bool_dynamic_value,
            nodiff_with_other_attrs,
            nodiff_each,
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
            native_backend_not_escaped
        ]},
        %% Raw-text elements (script/style/textarea/title): a dynamic content
        %% slot is markerless/render-once -- HTML comment diff markers would
        %% become literal content and corrupt the script/CSS/JSON-LD/title.
        {raw_text, [parallel], [
            raw_text_script_markerless,
            raw_text_script_not_escaped,
            raw_text_style_markerless,
            raw_text_json_ld_raw_verbatim,
            raw_text_title_escaped_markerless,
            raw_text_textarea_markerless,
            raw_text_render_once_no_diff,
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
            inline_comprehension_source
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
            invalid_element_binary_tag,
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
            invalid_child_mixed
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
            terminal_renders_styled_ansi,
            terminal_custom_sgr_attr,
            terminal_fg_attr,
            terminal_unknown_style_rejected,
            terminal_unknown_attr_rejected,
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

%% ?local: cannot be used in an az-nodiff template (no diff target to address).
local_in_nodiff_error(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_local_nodiff). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {'div', ['az-nodiff'], [arizona_template:local(<<\"k\">>, <<\"x\">>)]}"
        "    ). ",
        fun(R) -> R =:= local_in_nodiff end
    ).

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
        fun(R) -> R =:= local_html_only end
    ).

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

%% The native (terminal) backend produces plain text, not HTML, so value
%% interpolations are left bare -- escaping there would corrupt the output.
native_backend_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_esc_native). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native({'Text', [], [arizona_template:get(x, Bindings)]}). "
    ),
    T = Mod:render(#{x => <<"<not-escaped>">>}),
    [{_Az, Fun, _Loc}] = maps:get(d, T),
    ?assert(is_function(Fun, 0)),
    ?assertEqual(<<"<not-escaped>">>, Fun()).

%% A dynamic content slot inside <script> renders WITHOUT comment markers: the
%% browser would treat <!--az:...--> as literal script bytes (a module script's
%% HTML-comment tokens are a SyntaxError), so the slot must be markerless.
raw_text_script_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_script). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [{type, <<\"module\">>}], ["
        "        arizona_template:get(boot, Bindings)"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{boot => <<"import x from \"/c\"">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--/az-->")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"import x from \"/c\"")),
    ?assertEqual(<<"<script type=\"module\">import x from \"/c\"</script>">>, HTML).

%% A scalar inside <script> is emitted verbatim (NOT HTML-escaped): the parser
%% never decodes character references in raw text, so escaping `&`/`<` would
%% corrupt the JavaScript (`b=1&c=2` must not become `b=1&amp;c=2`).
raw_text_script_not_escaped(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_script_esc). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], [arizona_template:get(js, Bindings)]}). "
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
        "    arizona_template:html({'style', [], [arizona_template:get(css, Bindings)]}). "
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
raw_text_render_once_no_diff(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_once). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], [arizona_template:get(boot, Bindings)]}). "
    ),
    T1 = Mod:render(#{boot => <<"a">>}),
    {_HTML, Snap0} = arizona_render:render(T1),
    T2 = Mod:render(#{boot => <<"b">>}),
    {Ops, _Snap2} = arizona_diff:diff(T2, Snap0),
    ?assertEqual([], Ops),
    %% Production path: build a deps-carrying snapshot (as the live process does on
    %% mount), then diff/4 with the slot's binding explicitly marked changed.
    {_Ops0, DepsSnap, _V0} = arizona_diff:diff(T1, Snap0, #{}),
    {Ops4, _Snap4, _V1} = arizona_diff:diff(T2, DepsSnap, #{}, #{boot => true}),
    ?assertEqual([], Ops4).

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
        "        {'script', [], [arizona_template:get(boot, Bindings)]}, "
        "        {'span', [], [arizona_template:get(x, Bindings)]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{boot => <<"BOOT">>, x => <<"XV">>})),
    HTML = iolist_to_binary(HTML0),
    %% The script content is markerless; the span (after it) keeps its markers.
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<script>BOOT</script>")),
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
        "        {'script', [], [arizona_template:get(boot, Bindings)]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{boot => <<"BOOT">>, x => <<"XV">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(1, length(binary:matches(HTML, ~"<!--az:"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"-->XV<!--/az-->")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<script>BOOT</script>")).

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
        "        {'script', [], [arizona_template:get(boot, Bindings)]}, "
        "        {'p', [], [arizona_template:get(b, Bindings)]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(
        Mod:render(#{a => <<"AV">>, boot => <<"BOOT">>, b => <<"BV">>})
    ),
    HTML = iolist_to_binary(HTML0),
    %% Exactly two marked slots (span + p), none for the middle script.
    ?assertEqual(2, length(binary:matches(HTML, ~"<!--az:"))),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<script>BOOT</script>")),
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
        "        {'script', [], [arizona_template:get(boot, Bindings)]}, "
        "        {'span', [], [arizona_template:get(count, Bindings)]}"
        "    ]}). "
    ),
    T1 = Mod:render(#{boot => <<"a">>, count => <<"1">>}),
    {_HTML, Snap0} = arizona_render:render(T1),
    %% diff/2: changing the span binding emits an op; changing only the script does not.
    {OpsCount, _} = arizona_diff:diff(Mod:render(#{boot => <<"a">>, count => <<"2">>}), Snap0),
    ?assertNotEqual([], OpsCount),
    {OpsBoot, _} = arizona_diff:diff(Mod:render(#{boot => <<"b">>, count => <<"1">>}), Snap0),
    ?assertEqual([], OpsBoot),
    %% diff/4 (production): same, keyed by the dirty binding set.
    {_Ops0, DepsSnap, _V0} = arizona_diff:diff(T1, Snap0, #{}),
    {OpsCount4, _, _} = arizona_diff:diff(
        Mod:render(#{boot => <<"a">>, count => <<"2">>}), DepsSnap, #{}, #{count => true}
    ),
    ?assertNotEqual([], OpsCount4),
    {OpsBoot4, _, _} = arizona_diff:diff(
        Mod:render(#{boot => <<"b">>, count => <<"1">>}), DepsSnap, #{}, #{boot => true}
    ),
    ?assertEqual([], OpsBoot4).

%% Two dynamic content slots in one raw-text element: both render verbatim, both
%% markerless (the whole element's content is raw text).
raw_text_two_content_slots(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_two). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        arizona_template:get(a, Bindings), arizona_template:get(b, Bindings)"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{a => <<"AA">>, b => <<"BB">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<script>AABB</script>">>, HTML).

%% Static text around a dynamic slot inside a raw-text element survives in order,
%% markerless -- the computed value is spliced between the literal script bytes.
raw_text_static_text_around_slot(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_static). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        <<\"var x=\">>, arizona_template:get(url, Bindings), <<\"; init();\">>"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(Mod:render(#{url => <<"\"/u\"">>})),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<script>var x=\"/u\"; init();</script>">>, HTML).

%% A control-flow value (case) in a raw-text slot flows through
%% expand_block_element_tails: the selected branch renders verbatim and markerless,
%% and diffing is a no-op even when the scrutinee binding changes (render-once).
raw_text_conditional_value_markerless(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_rt_cond). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'script', [], ["
        "        case arizona_template:get(env, Bindings) of "
        "            prod -> <<\"prod()\">>; "
        "            _ -> <<\"dev()\">> "
        "        end"
        "    ]}). "
    ),
    T1 = Mod:render(#{env => prod}),
    {HTML0, Snap0} = arizona_render:render(T1),
    HTML = iolist_to_binary(HTML0),
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertEqual(<<"<script>prod()</script>">>, HTML),
    %% Render-once: switching the scrutinee emits no op (no marker to patch).
    {Ops, _} = arizona_diff:diff(Mod:render(#{env => dev}), Snap0),
    ?assertEqual([], Ops).

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
        "        [arizona_template:get(body, Bindings)]}). "
    ),
    T1 = Mod:render(#{src => <<"/a.js">>, body => <<"A">>}),
    {HTML0, Snap0} = arizona_render:render(T1),
    HTML = iolist_to_binary(HTML0),
    ?assertNotEqual(nomatch, binary:match(HTML, ~" az=\"")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"src=\"/a.js\"")),
    %% The body is markerless content even though the element has an az for the attr.
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    %% Changing the attribute emits a patch; changing only the content does not.
    {OpsSrc, _} = arizona_diff:diff(Mod:render(#{src => <<"/b.js">>, body => <<"A">>}), Snap0),
    ?assertNotEqual([], OpsSrc),
    {OpsBody, _} = arizona_diff:diff(Mod:render(#{src => <<"/a.js">>, body => <<"B">>}), Snap0),
    ?assertEqual([], OpsBody).

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
        "        {'script', [], [arizona_template:get(boot, Bindings)]}, "
        "        {'meta', [{name, arizona_template:get(m, Bindings)}]}"
        "    ]}). "
    ),
    {HTML0, _Snap} = arizona_render:render(
        Mod:render(#{t => <<"Hi">>, boot => <<"go()">>, m => <<"v">>})
    ),
    HTML = iolist_to_binary(HTML0),
    %% Neither raw-text slot is marked.
    ?assertEqual(nomatch, binary:match(HTML, ~"<!--az:")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<title>Hi</title>")),
    ?assertNotEqual(nomatch, binary:match(HTML, ~"<script>go()</script>")),
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
%% must handle scalar->template (OP_UPDATE) and template->scalar (OP_TEXT).
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

%% Test 48: template/1 with dynamic expression -- fallback path.
template1_dynamic_expr(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_dyn_expr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(arizona_template:get(content, Bindings, <<\"default\">>)). "
    ),
    T = Mod:render(#{content => <<"hello">>}),
    Fp = maps:get(f, T),
    ?assertEqual([<<>>, <<>>], maps:get(s, T)),
    Az0 = <<Fp/binary, "-0">>,
    [{Az0, {esc, Fun}, {pt_dyn_expr, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()).

%% Test 49: template/1 with dynamic expression -- render integration.
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
    ?assertEqual(<<"hello">>, iolist_to_binary(HTML)),
    %% Diff works
    Tmpl2 = Mod:render(#{content => <<"world">>}),
    {Ops, _} = arizona_diff:diff(Tmpl2, Snap),
    ?assertEqual([[0, Az0, <<"world">>]], Ops).

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
    StaticsBin = iolist_to_binary(maps:get(s, Tmpl)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az-nodiff">>)).

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
    ?assertEqual([<<"<p>">>, <<"</p>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {esc, Fun}, {pt_layout_elem, 1}}] = maps:get(d, T),
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
    ?assertEqual([<<"<div">>, <<">ok</div>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {attr, <<"class">>, Fun}, {pt_layout_dyn_attr, 1}}] = maps:get(d, T),
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
    ?assertEqual([<<"<p class=\"box\">Hello</p>">>], maps:get(s, T)),
    ?assertEqual([], maps:get(d, T)),
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
    ?assertEqual([<<"<p>">>, <<" and ">>, <<"</p>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [
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
    ?assertEqual([<<"<input">>, <<" />">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {attr, <<"value">>, Fun}, {pt_layout_void_dyn, 1}}] = maps:get(d, T),
    ?assertEqual(<<"hello">>, Fun()),
    %% No az attr in statics
    StaticsBin = iolist_to_binary(maps:get(s, T)),
    ?assertEqual(nomatch, binary:match(StaticsBin, <<"az=\"">>)).

%% Test 75: html with a single dynamic expression -- now uses standard html path.
layout_bare_dynamic(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_layout_bare_dyn). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html(maps:get(content, Bindings)). "
    ),
    T = Mod:render(#{content => <<"hi">>}),
    ?assertEqual([<<>>, <<>>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
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
    ?assertEqual([<<"prefix">>, <<"suffix">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    [{_Az, {esc, Fun}, {pt_layout_mixed_dyn, 1}}] = maps:get(d, T),
    ?assertEqual(<<"middle">>, Fun()),
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
    ?assertEqual(<<"prefixmiddlesuffix">>, HTML).

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
    HTML = iolist_to_binary(arizona_render:render_to_iolist(T)),
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
invalid_element_binary_tag(Config) when is_list(Config) ->
    assert_parse_error(
        "-module(pt_invalid_elem_bin). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:html("
        "        {<<\"div\">>, [], [<<\"hi\">>]}"
        "    ). ",
        fun
            (invalid_element) -> true;
            (_) -> false
        end
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

%% A named-ref callback whose body itself contains a nested ?each (with its own named ref):
%% both resolve via the module-global lookup and the recursive compile path.
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
    ?assert(is_map(Mod:render(#{sections => [#{items => [~"a"]}]}))).

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

%% A named-fun ?each inside ?native goes through the native_each rewrite: the callback is
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
    ?assertEqual(undefined, maps:get(single_root, Tmpl, undefined)).

%% A `?native` single-root list each IS flagged single_root -- the flag is a
%% backend-agnostic structural fact (one root node per item), stamped the same for
%% every target. The render target rides alongside it (`target => native`); the
%% diff engine, not the parse transform, uses the target to gate the web-only
%% `?OP_LIST_PATCH` (see arizona_diff:is_html_target/1 and the diff suite's
%% diff_list_native_target_full_update). Contrast each_single_root_flag (html: the
%% same single_root flag, no target key).
each_native_single_root_flag(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_each_native_sr). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:native_each(fun(Item) -> "
        "        {'Text', [], [maps:get(name, Item)]} "
        "    end, arizona_template:get(items, Bindings)). "
    ),
    Result = Mod:render(#{items => [#{name => <<"a">>}]}),
    Tmpl = maps:get(template, Result),
    ?assertEqual(true, maps:get(single_root, Tmpl, undefined)),
    ?assertEqual(native, maps:get(target, Tmpl, undefined)).

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
    ?assertEqual([<<"<p>">>, <<"</p>">>], maps:get(s, T)),
    ?assert(maps:is_key(f, T)),
    ?assertEqual(false, maps:get(diff, T)),
    [{undefined, {esc, Fun}, {pt_az_layout, 1}}] = maps:get(d, T),
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
    %% A ?terminal template carries `target => terminal`.
    Mod = compile_module(
        "-module(pt_term_marked). "
        "-export([render/1]). "
        "render(Bindings) -> "
        "    arizona_template:terminal("
        "        {line, [green], [arizona_template:get(msg, Bindings)]}"
        "    ). "
    ),
    T = Mod:render(#{msg => ~"hi"}),
    ?assertEqual(terminal, maps:get(target, T)).

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
with_handoff_freezes_without_tracking(Config) when is_list(Config) ->
    Mod = compile_module(
        "-module(pt_with_freeze). "
        "-export([render/1, inner/1]). "
        "render(Bindings) -> "
        "    arizona_template:html({'p', [], [inner(Bindings)]}). "
        "inner(B) -> "
        "    arizona_template:html({'span', [], [arizona_template:get(x, B)]}). "
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
