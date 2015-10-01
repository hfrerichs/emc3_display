;-----------------------------------------------------------------------
; module:	load_data
; date:		Jul 17th, 2007
;
; author:	Heinke Frerichs
; email:	h.frerichs@fz-juelich.de
;
; description:	provides loading data routines
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
pro load_data_template, lun, physQ
	readf, lun, physQ
end; pro load_data_template
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro load_data_templateZ, lun, isp, physQ
	dummy	= 0.0
        for i=0,isp do begin
		readf, lun, dummy
		readf, lun, physQ
	endfor
end; pro load_data_template
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro load_ti, lun,  physQ
	readf, lun, physQ
	readf, lun, physQ
end
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
pro load_ni, lun,  physQ
	dummy	= 0.0
	readf, lun, dummy
	readf, lun, physQ
end
;-----------------------------------------------------------------------


;-----------------------------------------------------------------------
pro load_data, machine, shot, casex, run, data_id, sub_id, physQ, species_id=species_id
	common	config_cb
	common	grid_info_cb
	common	grid_data_cb


; set data file
	base_dir	= sim_dir+'/'+machine+'/'+shot
	case (data_id) of
	0:	data_file	= base_dir+'/'+casex+'/'+run+'/'+plasma_arr[1,sub_id]
	2:	data_file	= base_dir+'/'+geometry_dir+'/'+magnetic_arr[1,sub_id]
	3:	data_file	= base_dir+'/'+casex+'/'+run+'/'+diagnostic_arr[1,sub_id]
	4:	data_file	= base_dir+'/'+casex+'/'+run+'/'+impurity_arr[1,sub_id]
	endcase


; prepare data array
	physQ	= make_array(nc_pl, /fl)			; scalar data on plasma/physical cells (standard)
	;physQ	= make_array(ncell_n1, /fl)			; scalar data on plasma/physical+neutral cells (standard)
	if (data_id eq 3) then begin
	case (diagnostic_arr[4,sub_id]) of
	'p'	: physQ	= make_array(nc_pl, /fl)		; scalar data on plasma/physical cells (standard)
	'n'	: physQ	= make_array(ncell_n1, /fl)		; scalar data on plasma+neutral cells
	'm'	: physQ	= make_array(mesh_p_os(nzonet), /fl)	; scalar data on magnetic cells
	'm3'	: physQ	= make_array(mesh_p_os(nzonet), 3, /fl)	; vector data on magnetic cells
	endcase
	endif


; check if data file exists
	if not file_test(data_file) then begin
		print, "warning: data file doesn't exist: ", data_file
		physQ(*)	= 0.0
		return
	endif


; read data
	openr, lun, data_file, /get_lun

	case (data_id) of
	; plasma data
	0: begin
		case sub_id of
		1:	load_ti, lun, physQ
		2:	load_ni, lun, physQ
		else:	load_data_template, lun, physQ
		endcase
	end
	1:
	; magnetic data
	2: begin
		case sub_id of
		0:	read_flux_conservation, lun
		else:	load_data_template, lun, physQ
		endcase
	end
	; diagnostic data
	3:	load_data_template, lun, physQ
	; impurity data
	4:	load_data_templateZ, lun, species_id, physQ
	endcase

	free_lun, lun

	physQ	= process_data(physQ)
end; pro load_data
;-----------------------------------------------------------------------
