FUNCTION-POOL zfgppm.                       "MESSAGE-ID ..

CONSTANTS: con_act       VALUE 'A',
           con_plan      VALUE 'P',
           con_cmt       VALUE 'C',
           con_spa       VALUE ' ',
           con_on        TYPE c VALUE 'X',
           c_first_day   TYPE numdy VALUE '01',
           c_koae(4)     TYPE c VALUE 'KAOE',
           c_chart(2)    TYPE c VALUE 'LD',
           c_language(1) TYPE c VALUE 'E'.

CONSTANTS: BEGIN OF con_item_group,
             budget    TYPE kaep_item_group VALUE 'BDG',
             strc_plan TYPE kaep_item_group VALUE 'STPL',

           END OF con_item_group.


CONSTANTS: BEGIN OF con_field,
             kstar TYPE kaep_fieldname VALUE 'KSTAR',
           END   OF con_field.


TYPES: BEGIN OF ty_elm_ps_sort_by_objnr,
         objnr TYPE j_objnr,
         line  TYPE sy-tabix,
       END OF   ty_elm_ps_sort_by_objnr.


TYPES: BEGIN OF ty_cost_element_descr,
         saknr TYPE skat-saknr,
         txt50 TYPE skat-txt50,
       END OF    ty_cost_element_descr.


DATA:
  gt_ppm_cost     TYPE TABLE OF zkaep_t_covp_ext,
  gt_elm_ps_objnr TYPE STANDARD TABLE OF ty_elm_ps_sort_by_objnr,
  gv_repid        LIKE sy-repid            VALUE 'RKPEP003',
  gv_repid_f      TYPE kaep_disvariant_key VALUE 'RKPEP000',
  gv_handle       TYPE i                   VALUE 3000,
  gv_actual_v     LIKE covp-versn          VALUE '000',
  gv_fixvar       TYPE c                   VALUE space,
  gv_called       TYPE c                   VALUE space,
  gv_project      TYPE ps_pspid.