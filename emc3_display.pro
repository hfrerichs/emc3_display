;-----------------------------------------------------------------------
; program:	emc3_display
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
;-----------------------------------------------------------------------

@config
@tools
@event_ids
@emc3_grid
@select_run
@settings
@fl_profiles
@iso_elements
@plot_data
@overlay_data
@target
;@Textor/plot_target
@1D_profiles

pro emc3_display, EMC3_DISPLAY_DIR
	; load configuration
	load_config, EMC3_DISPLAY_DIR

	; set event id definitions
	event_ids

	; init select_run, select_data, emc3_grid routines
	;emc3_grid
	;plot_data
	flux_conservation
	;select_run
	init_grid_module
	loadct, 39

	main_window
end; pro emc3_display
;-----------------------------------------------------------------------
