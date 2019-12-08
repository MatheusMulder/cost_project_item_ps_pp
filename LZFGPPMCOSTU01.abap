FUNCTION zk_line_items_select_and_list.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_CALLING_REPORT) LIKE  SY-REPID
*"     VALUE(I_ITEM_GROUP) TYPE  KAEP_ITEM_GROUP
*"     VALUE(I_REP_OBJECT) TYPE  KAEP_REP_OBJECT
*"     VALUE(I_DISPLAY_VARIANT) TYPE  KAEP_DISVARIANT OPTIONAL
*"     VALUE(I_CO_AREA) LIKE  TKA01-KOKRS
*"     VALUE(I_ACTUAL_VERSION) LIKE  TKA09-VERSN DEFAULT '000'
*"     VALUE(IT_SELECTION) TYPE  KAEP_T_COSEL
*"     VALUE(IT_FREE_SELECTION) TYPE  RSDS_TWHERE OPTIONAL
*"     VALUE(IT_FREE_SELECTION_RANGE) TYPE  RSDS_TRANGE OPTIONAL
*"     VALUE(I_EXIT_SET_TITLE) TYPE  KAEP_FORMNAME OPTIONAL
*"     VALUE(I_EXIT_USER_COMMAND) TYPE  KAEP_FORMNAME OPTIONAL
*"     VALUE(I_SHOWNOZERO) TYPE  KAEP_SETT-SHOWNOZERO OPTIONAL
*"     VALUE(I_FIX_VARIANT) LIKE  KAEP_SETT-FIXVAR OPTIONAL
*"     VALUE(I_ONLINE_MAXSEL) LIKE  KAEP_SETT-MAXSEL DEFAULT 10000
*"     VALUE(IT_GROUPS) TYPE  KAEP_T_GROUPS OPTIONAL
*"     VALUE(I_CALLED_WITH_DATA) TYPE  KAEP_FLAG DEFAULT SPACE
*"     VALUE(I_CALLED_BY_RRI) TYPE  KAEP_FLAG DEFAULT SPACE
*"     VALUE(I_UPDATE) TYPE  KAEP_FLAG DEFAULT SPACE
*"     VALUE(IT_PD_SELECTION) TYPE  KAEP_T_COSEL OPTIONAL
*"     VALUE(I_USEDB) LIKE  DTINP-XUSEAR DEFAULT 'X'
*"     VALUE(I_USEAR) LIKE  DTINP-XUSEAR DEFAULT SPACE
*"     VALUE(I_AROBJ) LIKE  ARCH_OBJ-OBJECT OPTIONAL
*"     VALUE(I_TRANSFER_VERSIONS) TYPE  KAEP_FLAG DEFAULT 'X'
*"     VALUE(I_NO_DISPLAY) TYPE  C DEFAULT 'X'
*"     VALUE(I_USE_GRID) TYPE  KAEP_SETT-USEGRD DEFAULT 'X'
*"     VALUE(I_TCODE) TYPE  SY-TCODE OPTIONAL
*"     VALUE(I_TREE_DISPLAY) TYPE  C OPTIONAL
*"     VALUE(I_CONTAINER) TYPE REF TO  CL_GUI_CUSTOM_CONTAINER OPTIONAL
*"     VALUE(I_HANA_ACTIVE) TYPE  C OPTIONAL
*"     VALUE(I_TOP_OF_PAGE_CONTAINER) TYPE REF TO
*"        CL_GUI_CUSTOM_CONTAINER OPTIONAL
*"     VALUE(I_GROUP_OBJ) TYPE  C DEFAULT 'X'
*"     VALUE(I_SOH_ACTIVE) TYPE  C OPTIONAL
*"  EXPORTING
*"     REFERENCE(T_KAEP_T_COVP_EXT) TYPE  ZTTKAEP_T_COVP_EXT
*"  TABLES
*"      T_ARSEL STRUCTURE  RNG_ARCHIV OPTIONAL
*"  CHANGING
*"     VALUE(IT_KAEPX_DATA_CARRIER) TYPE  KAEPX_DATA_CARRIER_T OPTIONAL
*"     VALUE(ER_ALV_GRID) TYPE REF TO  CL_GUI_ALV_GRID OPTIONAL
*"  EXCEPTIONS
*"      UNKNOWN_ITEM_GROUP
*"      NO_DATA_SELECTED
*"----------------------------------------------------------------------
  DATA: ls_text_flags     TYPE kaep_text_flags,
        ls_disvariant_key TYPE kaep_disvariant_key,
        ls_layout         TYPE slis_layout_alv,
        ls_selection      TYPE kaep_cosel,
        ld_subrc          LIKE sy-subrc,
        ld_con_hex02      TYPE x VALUE '02'.

*.in case of container handling do the refresh atthe beginning not at
*.the end
  IF NOT i_container IS INITIAL.
    PERFORM refresh_hana.
  ENDIF.

  gd-use_alv_grid = i_use_grid.
  CLEAR gd_extern.                                          "HW763206

  IF sy-tcode IS INITIAL.                                   "802740
    gd_tcode = i_tcode.                                     "802740
  ENDIF.                                                    "802740

  LOOP AT it_selection INTO ls_selection WHERE field = 'INFOSYS'.
    IF NOT ls_selection-low IS INITIAL.
      gd-infosys = 'X'.
    ENDIF.
  ENDLOOP.

*.global data for kaepx
  gt_kaepx_data_carrier[] = it_kaepx_data_carrier[].
  gd_usear                = i_usear.
  gt_it_selection         = it_selection.
  gd_fix_variant          = i_fix_variant.
  gd_group_obj            = i_group_obj.

* always only select shown fields if it is batch or start with printing
  IF sy-subty O ld_con_hex02 OR sy-batch = 'X'.
    i_fix_variant = con_on.
    CLEAR gd-use_alv_grid.
  ENDIF.

  gd-usear = i_usear.

  gd_soh_active = i_soh_active.
  gt_free_selection = it_free_selection.

* initialize global parameters (including coding fieldcat)
  IF i_calling_report IS INITIAL OR
     i_rep_object = con_rep_object-allocations.
    ls_disvariant_key-report    = self.
  ELSE.
    ls_disvariant_key-report    = i_calling_report.
  ENDIF.
  ls_disvariant_key-handle    = i_item_group.
  ls_disvariant_key-log_group = i_rep_object.
  ls_disvariant_key-variant   = i_display_variant.

* ls_layout-max_linesize = sy-linsz.                           "1277539

* check if message handling is active, if not activate it
  CALL FUNCTION 'MESSAGES_ACTIVE'
    EXCEPTIONS
      not_active = 1.
  IF sy-subrc <> 0.
    CALL FUNCTION 'MESSAGES_INITIALIZE'.
  ELSE.                                                     "HW763206
    gd_extern = abap_true.                                  "HW763206
  ENDIF.

  PERFORM message_set_defaultline USING '1'.

  PERFORM init_globals USING ls_disvariant_key
                             i_shownozero                  "COGM 1671025
                             i_fix_variant
                             i_online_maxsel
                             it_selection
                             i_calling_report
                             i_exit_user_command
                             i_exit_set_title
                             i_co_area
                             i_actual_version
                             i_called_with_data
                             i_called_by_rri
                             i_update
                             i_usear
                             i_transfer_versions
                       CHANGING gd
                                ls_layout.

  PERFORM init_alv_events USING i_fix_variant.

  PERFORM read_disvariant USING    ls_layout
                          CHANGING ls_disvariant_key.

  PERFORM message_set_defaultline USING '2'.


  IF NOT gt_kaepx_data_carrier[] IS INITIAL.
    REFRESH: <gt_head_data>, <gt_pos_data>.
    REFRESH gt_kaepx_data_carrier.
  ENDIF.
*
*****************************************************************
**..determine selection out of variant....
****************************************************************

  PERFORM select USING it_selection
                     it_free_selection
                     i_fix_variant
                     i_online_maxsel.

  t_kaep_t_covp_ext = gt_covp_ext.

  REFRESH: <gt_head_data>, <gt_pos_data>.

ENDFUNCTION.