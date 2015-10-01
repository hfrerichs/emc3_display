;-----------------------------------------------------------------------
pro load_config, EMC3_DISPLAY_DIR
	common	config_cb, $
		root_dir, CT_file, $
		sim_dir, $
		geometry_dir, emc3_dir, eirene_dir, postproc_dir, diag_dir, $
		grid_info_file, grid_data_file, mag_axis_data_file, physical_cell_data_file, $
		case_info_file, note_file, run_exec_dir, $
		fl_profiles_file, $
; runtime variables (set during run selection)
		current_geometry_dir, $
		last_machine, last_shot, last_case, last_run, $
		plasma_arr, magnetic_arr, impurity_arr, diagnostic_arr, depo_arr, depo_file, $
		machines, $
		base_id, overlay_id, d3d_pref, textor_pref, default_pref, $
		fieldline_ui, loaded_shot_fl, tb_overlay_data, $
		zooms, nzoom, zoom_str, zoom_id

	root_dir	= EMC3_DISPLAY_DIR
	CT_file		= 'COLOR_TABLES'
	sim_dir		= getenv('sim_dir')
	print, 'using simulation directory: ', sim_dir
	geometry_dir		= getenv('geometry_dir')
	emc3_dir		= getenv('emc3_output_dir')
	eirene_dir		= getenv('eirene_output_dir')
	postproc_dir		= getenv('postproc_dir')
	diag_dir		= getenv('diagnostics_dir')
	grid_info_file		= getenv('grid_info_file')
	grid_data_file		= getenv('grid_data_file')
	mag_axis_data_file	= 'mag_axis.dat'
	physical_cell_data_file	= getenv('physical_cell_data_file')
	case_info_file		= getenv('case_info_file')
	note_file		= 'notes.txt'
	run_exec_dir		= getenv('run_exec_dir')
	fl_profiles_file	= 'PARALLEL_PLOTTING.txt'
	user_conf_dir		= getenv('HOME')+'/.emc3_display/'

	; load history
	history		= user_conf_dir+'history'
	last_machine	= ''
	last_shot	= ''
	last_case	= ''
	last_run	= ''
	fqp		= file_search('.', /FULLY_QUALIFY_PATH)
	if (strmatch(fqp, '*'+sim_dir+'*')) then begin
		print, 'setting run from working directory'
		pos	= stregex(fqp, sim_dir, length=len)
		setup	= strsplit(strmid(fqp, pos+len+1), '/', /extract)
		n	= size(setup, /dimension)
		switch n of
			4: last_run	= setup(3)
			3: last_case	= setup(2)
			2: last_shot	= setup(1)
			1: last_machine	= setup(0)
		endswitch
	endif else if (file_test(history)) then begin
		print, 'setting run from history'
		openr, lun, history, /get_lun
		readf, lun, last_machine
		readf, lun, last_shot
		readf, lun, last_case
		readf, lun, last_run
		free_lun, lun
	endif

	; main plasma data
	plasma_arr	= [ $
; title				data file			symbol		units
['electron temperature',	emc3_dir+'/TE_TI',		'T_e',		'eV'], $
['ion temperature',		emc3_dir+'/TE_TI',		'T_i',		'eV'], $
['density',			emc3_dir+'/DENSITY',		'n_e',		'10^19 m^-3'], $
['mach number',			emc3_dir+'/MACH_NUMBER',	'z_mach',	''], $
['radiated power',		emc3_dir+'/SOURCE_RAD_E',	'P_rad',	'W cm^-3'], $
['radiated power',		emc3_dir+'/IMP_RADIATION',	'P_rad',	'W cm^-3'], $
['radiated power (A)',		emc3_dir+'/IMP_RADIATION_A',	'P_rad',	'W cm^-3'], $
['radiated power (B)',		emc3_dir+'/IMP_RADIATION_B',	'P_rad',	'W cm^-3'], $
['H density',			eirene_dir+'/DENSITY_A',	'n_H',		'cm^-3'], $
['H2 density',			eirene_dir+'/DENSITY_M',	'n_H2',		'cm^-3'], $
['H energy density',		eirene_dir+'/DENSITY_E_A',	'E_H',		'eV cm^-3'], $
['H2 energy density',		eirene_dir+'/DENSITY_E_M',	'E_H2',		'eV cm^-3'], $
['particle source',		eirene_dir+'/SOURCE_P',		'S_p',		'A cm^-3'], $
['impurity source',		eirene_dir+'/SOURCE_P2',	'S_imp',	'A cm^-3'], $
['impurity0 density',		eirene_dir+'/DENSITY_A2',	'n_imp0',	'cm^-3'], $
['momentum source',		eirene_dir+'/SOURCE_M',		'S_m',		'A g cm^-2 s^-1'], $
['el. energy source',		eirene_dir+'/SOURCE_E_E',	'S_ee',		'...'], $
['ion energy source',		eirene_dir+'/SOURCE_E_I',	'S_ei',		'...'], $
['Te variance',			emc3_dir+'/STABW_TE',		'delta T_e',	'...'], $
['Ti variance',			emc3_dir+'/STABW_TI',		'delta T_i',	'...'], $
['# trajectories',		emc3_dir+'/TRAJECTORIES',	'N_traject',	''], $
['el. potential',		emc3_dir+'/POTENTIAL',		'phi',		'...'], $
['el. field',			postproc_dir+'/E_FIELD',	'E',		'...'], $
['H alpha',			postproc_dir+'/H_ALPHA',	'P_Halpha',	'W cm^-3'], $
['Lyman alpha',			postproc_dir+'/LYMAN_ALPHA',	'P_Ly-alpha',	'W cm^-3'], $
['Te gradient',			postproc_dir+'/Grad_Te',	'Grad Te',	'eV/cm'], $
['Ti gradient',			postproc_dir+'/Grad_Ti',	'Grad Ti',	'eV/cm'], $
['tor. cell range',		postproc_dir+'/T_RANGE',	't_range',	'1']]

	; magnetic data
	magnetic_arr	= [ $
; title				data file			axis label
['flux conservation',		'flux_conservation',		'flux conservation',	'%'], $
['connection length',		'connection_length',		'L_c',			'cm'], $
['poloidal turns',		'poloidal_turns',		'L_pt',			'p.t.'], $
['shortest connection length',	'connection_length_shortest',	'L_cs',			'cm'], $
['forward connection length',	'connection_length_fw',		'L_fw',			'cm'], $
['backward connection length',	'connection_length_bw',		'L_bw',			'cm'], $
['shortest poloidal turns',	'poloidal_turns_shortest',	'L_pts',		'p.t.'], $
['field line average Psi',	'PSI_AV',			'Psi_av',		''], $
['toroidal cell range',		'T_RANGE',			't_range',		'1'], $
['poloidal cell range',		'P_RANGE',			'p_range',		'1'], $
['radial cell range',		'R_RANGE',			'r_range',		'1']]

	; impurity data
	impurity_arr	= [ $
; title				data file			symbol		units
['density',			emc3_dir+'/DENSITY',		'density',	'cm^-3'], $
['loss region coef',		diag_dir+'/LOSS_REGION_COEF',	'l_coef',	''], $
['velocity',			emc3_dir+'/VELOCITY',		'velocity',	'cm/s'], $
['pressure force',		postproc_dir+'/F_pa',		'F_pa',		'N'], $
['friction force',		postproc_dir+'/F_fr',		'F_fr',		'N'], $
['parallel E force',		postproc_dir+'/F_el',		'F_el',		'N'], $
['el. thermal force',		postproc_dir+'/F_th_e1',	'F_the',	'N'], $
['ion thermal force',		postproc_dir+'/F_th_i1',	'F_thi',	'N'], $
['velocity post proc',		postproc_dir+'/V_i',		'velocity',	'cm/s']]

	; diagnostic data
	diagnostic_arr	= [ $
; title				data file			symbol		units		phys/mag
['H density',			eirene_dir+'/DENSITY_A',	'n_H',		'cm^-3',	'n'], $
['H2 density',			eirene_dir+'/DENSITY_M',	'n_H2',		'cm^-3',	'n'], $
['toroidal cell range',		diag_dir+'/T_RANGE',	't_range',	'',		'p'], $
['radial cell range',		diag_dir+'/R_RANGE',	'r_range',	'',		'p'], $
['poloidal cell range',		diag_dir+'/P_RANGE',	'p_range',	'',		'p'], $
['toroidal cell range (m)',	diag_dir+'/T_RANGEm',	't_range',	'',		'm'], $
['radial cell range (m)',	diag_dir+'/R_RANGEm',	'r_range',	'',		'm'], $
['poloidal cell range (m)',	diag_dir+'/P_RANGEm',	'p_range',	'',		'm'], $
['velocity',			postproc_dir+'/VELOCITY',	'velocity',	'cm/s',		'p'], $
;['radial cell range',		diag_dir+'/NR_CELL',		'nr_cell',	'',		'm'], $
;['poloidal cell range',		diag_dir+'/NP_CELL',		'np_cell',	'',		'm'], $
;['el. mean free path',		diag_dir+'/E_FREEMEANPATH',	'lambda_e',	'cm',		'p'], $
['el. mean free path',		diag_dir+'/lambda_e',		'lambda_e',	'cm',		'p'], $
['r test',			diag_dir+'/r_test',		'r_test',	'',		'p'], $
['mom. src. from transport',	emc3_dir+'/ALFA_SOURCE',	'M_src',	'...',		'p'], $
['q_cond',			emc3_dir+'/QCOND',		'q_cond',	'W / cm^2','p'], $
['dSe_dTe',			eirene_dir+'/DSE_DTE',		'DSE_DTE',	'','p'], $
['sample denstity test',	postproc_dir+'/H8_test_DENS',	'n_e',		'cm^-3',	'p'], $
['sample denstity test',	postproc_dir+'/H8_test_DENS',	'n_e',		'cm^-3',	'p'], $
;['grad x density test',         postproc_dir+'/Grad_test_DENSX','dn_e / dx',    'cm^-4',	'm'], $
;['grad y density test',         postproc_dir+'/Grad_test_DENSY','dn_e / dy',    'cm^-4',	'm'], $
;['grad z density test',         postproc_dir+'/Grad_test_DENSZ','dn_e / dz',    'cm^-4',	'm'], $
;['grad x psi_pol test',         postproc_dir+'/Grad_test_PSIX', 'dpsi / dx',    'cm^-1',	'm'], $
;['grad y psi_pol test',         postproc_dir+'/Grad_test_PSIY', 'dpsi / dy',    'cm^-1',	'm'], $
;['grad z psi_pol test',         postproc_dir+'/Grad_test_PSIZ', 'dpsi / dz',    'cm^-1',	'm'], $
;['radial density gradient',	postproc_dir+'/Grad_ne_psi',    'dne / de_psi', 'cm^-4',	'm'], $
['electrostatic potential',	postproc_dir+'/POTENTIAL',	'Phi',		'eV',		'p'], $
['current density',		postproc_dir+'/CURRENT_VOL',	'j',		'A cm^-2',	'p'], $
['Field line count',		diag_dir+'/Fl_count',		'n_fl',		'',	'p'], $
['div gamma_p_para',		postproc_dir+'/DIV_GAMMA_P_PARA',	'div Gp_para',	'A cm^-3',	'm'], $
['div gamma_p_perp',		postproc_dir+'/DIV_GAMMA_P_PERP',	'div Gp_perp',	'A cm^-3',	'm'], $
['particle balance',		postproc_dir+'/SUM_P',		'p balance',	'A cm^-3',	'p'], $
['diff. particle flux',		postproc_dir+'/GAMMA_P_DIFF',   'gamma_p_diff', 'cm^-2 s^-1',	'm'], $
['pressure gradient',		postproc_dir+'/GRAD_P',		'grad p',	'Te cm^-4',	'p'], $
['R gradient',			postproc_dir+'/GRAD_R',		'grad R',	'1',		'm'], $
['Istatu',			diag_dir+'/ISTATU',		'Istatu',	'1',		'm'], $
;['pressure',			postproc_dir+'/PRESSURE',	'p',		'Te cm^-3',	'p'], $
['diamagnetic drif',		postproc_dir+'/V_DIA',		'v_dia',	'cm / s',	'p'], $
['norm. poloidal flux',         postproc_dir+'/Psi_test',       'psi_pol',      '',		'm'], $
['H ion. source',		postproc_dir+'/V_SOURCE_H',	'V_src_H',	'cm^-3 s^-1',	'p'], $
['H+ rec. sink',		postproc_dir+'/V_SINK_H',	'V_sink_H',	'cm^-3 s^-1',	'p'], $
['H+ rec. (EMC3)',		emc3_dir+'/H_VOLREC',		'vol. rec. H+',	'cm^-3 s^-1',	'p'], $
['H+ rec. (EIRENE)',		eirene_dir+'/VREC_DIAG',	'vol. rec. H+',	'cm^-3 s^-1',	'p'], $
['Ion. / Rec. ratio',	postproc_dir+'/IR_RATIO','R',		'',		'p'], $
['loss region flag',		diag_dir+'/LOSS_REGION_FLAG',	'l_flag',	'',		'p'], $
['beta_limit',			diag_dir+'/FL_FACTOR',	'beta_limit',	'',		'p'], $
['q_classical',			diag_dir+'/Q_CLASS',	'q_classical',	'',		'p'], $
['q_limit',			diag_dir+'/Q_LIMIT',	'q_limit',	'',		'p'], $
['smooth Te',			diag_dir+'/SMOOTH_TE',	'smooth_Te',	'',		'p'], $
['beta_HC',			diag_dir+'/beta_HC',	'beta_HC',	'',		'p'], $
['lpara(1pt)',			diag_dir+'/L1PARA',	'lpara(1pt)',	'',		'p'], $
;['cond./conv. E heatflux',	diag_dir+'/FL_CONDUCTIVE_DIV_CONVECTIVE_HEATFLUX',	'ratio',	'',	'p'], $
['fcond',			diag_dir+'/fcond',		'f_cond',	'',	'p'], $
['fmom',			diag_dir+'/fmom',		'f_mom',	'',	'p'], $
['fmom1',			diag_dir+'/fmom1',		'f_mom1',	'',	'p'], $
['fmom2',			diag_dir+'/fmom2',		'f_mom2',	'',	'p'], $
['magnetic flux',		diag_dir+'/MAGNETIC_FLUX',	'magnetic_flux',	'',	'm'], $
['magnetic flux cons.',		diag_dir+'/MAGNETIC_FLUX_CONSERVATION',	'magnetic_flux_cons',	'',	'm'], $
['magnetic flux cons.',		diag_dir+'/FLUX_CONSERVATION_CELL',	'magnetic_flux_cons',	'',	'p'], $
['field strength',		diag_dir+'/FIELD_STRENGTH',	'field_strength',	'',	'm'], $
['nsplit',			diag_dir+'/nsplit_level',	'nsplit',	'',	'p'], $
['adiabatic cooling el.',	diag_dir+'/SOURCE_E_KIN_E',	'ad. cool. el.','...',		'p'], $
['adiabatic cooling ion',	diag_dir+'/SOURCE_E_KIN_I',	'ad. cool. ion','...',		'p'], $
['grad. para. Te smooth',	emc3_dir+'/PAR_DERIV_TE',	'grad. para. Te','eV / cm',	'p'], $
['grad. para. Te',		emc3_dir+'/PAR_DERIV_TI',	'grad. para. Te','eV / cm',	'p'], $
;['grad. para. Te',		diag_dir+'/GRAD_PARA_Te',	'grad. para. Te','eV / cm',	'p'], $
;['grad. para. Ti',		diag_dir+'/GRAD_PARA_Ti',	'grad. para. Ti','eV / cm',	'p'], $
;['grad. para. ne',		diag_dir+'/GRAD_PARA_ne',	'grad. para. ne','cm^-3 / cm',	'p'], $
;['grad. para. Zm',		diag_dir+'/GRAD_PARA_Zm',	'grad. para. Zm','1 / cm',	'p'], $
['D_perp',			diag_dir+'/DPERP',		'D_perp',	'cm^2 / s',	'p'], $
['Xe_perp',			diag_dir+'/XEPERP',		'Xe_perp',	'cm^2 / s',	'p'], $
['Xi_perp',			diag_dir+'/XIPERP',		'Xi_perp',	'cm^2 / s',	'p'], $
['Grad_R D_perp',		diag_dir+'/GRADR_DPERP',	'Grad_R D_perp','cm / s',	'm'], $
['Grad_Z D_perp',		diag_dir+'/GRADZ_DPERP',	'Grad_Z D_perp','cm / s',	'm'], $
['Grad_Phi D_perp',		diag_dir+'/GRADPHI_DPERP',	'Grad_Phi D_perp','cm^2 / s / rad',	'm'], $
['cell volume',			diag_dir+'/VOL3D',		'V_cell',	'cm^-3',	'm'], $
['Cells with plate surfaces',	postproc_dir+'/plates_check.txt', 'N_plate',	'',		'p'], $
['En jumpstep polo',		diag_dir+'/PERP_JUMPSTEP_FRACTION_POL', 'j_pol','',		'p'], $
['En jumpstep radi',		diag_dir+'/PERP_JUMPSTEP_FRACTION_RAD', 'j_rad','',		'p'], $
['En jumpstep para conv',	diag_dir+'/PAR_JUMPSTEP_FRACTION_CONV', 'j_conv','',		'p'], $
['En jumpstep para diff',	diag_dir+'/PAR_JUMPSTEP_FRACTION_DIFF', 'j_diff','',		'p'], $
['parallel cell length',	diag_dir+'/PAR_CELL_LENGTH',		'n_para','cells',	'p'], $
['radial timestep',		diag_dir+'/timestep_rad', 		'tau_rad','',		'p'], $
['poloidal timestep',		diag_dir+'/timestep_pol', 		'tau_pol','',		'p'], $
['parallel diff. timestep',	diag_dir+'/timestep_para_diff', 	'tau_para_diff','',		'p'], $
['parallel conv. timestep',	diag_dir+'/timestep_para_conv', 	'tau_para_conv','',		'p'], $
['PxPL_2',			diag_dir+'/PxPL_2_test.txt', 	'PxPL_2',	'',		'p'] $
;['transport coeff. 1 0',	diag_dir+'/TRANS_COEFF_10',	'tc_10',	'',		'p'], $
;['transport coeff. 2 0',	diag_dir+'/TRANS_COEFF_20',	'tc_20',	'',		'p'], $
;['transport coeff. 3 0',	diag_dir+'/TRANS_COEFF_30',	'tc_30',	'',		'p'], $
;['transport coeff. 4 0',	diag_dir+'/TRANS_COEFF_40',	'tc_40',	'',		'p'], $
;['transport coeff. 5 0',	diag_dir+'/TRANS_COEFF_50',	'tc_50',	'',		'p'], $
;['transport coeff. 6 0',	diag_dir+'/TRANS_COEFF_60',	'tc_60',	'',		'p'], $
;['transport coeff. 7 0',	diag_dir+'/TRANS_COEFF_70',	'tc_70',	'',		'p'], $
]


	; deposition data
	depo_arr	= [ $
['Particle Flux Density',					'gamma_p',		'kA / m^2'], $
['Heat Flux Density',						'gamma_e',		'MW / m^2'], $
['ne in front of wall',						'ne',			'cm^-3'], $
['Te in front of wall',						'Te',			'eV'], $
['Ti in front of wall',						'Ti',			'eV'], $
['CL in front of wall',						'CL',			'm'], $	; scaled in target.pro
['Number of MC-Particles hitting wall',				'n_MC',			'1']]
	depo_file	= ['TARGET_ELEMENT_INFO', 'TARGET_GEO_INFO', 'TARGET_INFO']


;===============================================================================
	; machine preferences
	machines	= ['DIII-D', 'Textor']
	tmp		= {RZsize:indgen(2), trsize:indgen(2), tpsize:indgen(2)}

	d3d_pref	= tmp
	d3d_pref.RZsize(0)	= 800	; X-plot window size for R-Z plot
	d3d_pref.RZsize(1)	= 950
	d3d_pref.trsize(0)	= 800	; X-plot window size for theta-rmin plot
	d3d_pref.trsize(1)	= 550
	d3d_pref.tpsize(0)	= 800	; X-plot window size for theta-psi plot
	d3d_pref.tpsize(1)	= 480

	textor_pref	= tmp
	textor_pref.RZsize(0)	= 800	; X-plot window size for R-Z plot
	textor_pref.RZsize(1)	= 640
	textor_pref.trsize(0)	= 800	; X-plot window size for theta-rmin plot
	textor_pref.trsize(1)	= 550
	textor_pref.tpsize(0)	= 800	; X-plot window size for theta-psi plot
	textor_pref.tpsize(1)	= 480

	default_pref	= textor_pref

;===============================================================================
	; pre-defined zooms (for specific machines)
	conf_file	= root_dir+'/zoom.conf'
	openr, lun, conf_file, /get_lun
	nzoom = 0
	zooms	= {t_zoom, machine:'DIII-D', tag:'tagname', phi:0.0, Rmin:100.0, Rmax:160.0, Zmin:-140.0, Zmax:-80.0}
	readf, lun, nzoom
	print, 'loading ', nzoom, ' pre-defined zoom-ins'
	if (nzoom GT 0) then zooms=replicate(zooms,nzoom)
	s = ' '
	r1 = 0.0
	r2 = 0.0
	for i=0,nzoom-1 do begin
		readf, lun, s
		zooms[i].machine = s
		readf, lun, s
		zooms[i].tag = s
		readf, lun, r1
		zooms[i].phi = r1
		readf, lun, r1, r2
		zooms[i].Rmin = r1
		zooms[i].Rmax = r2
		readf, lun, r1, r2
		zooms[i].Zmin = r1
		zooms[i].Zmax = r2
		;readf, lun, zooms[i].machine
		;readf, lun, zooms[i].tag
		;readf, lun, zooms[i].phi
		;readf, lun, zooms[i].Rmin, zooms[i].Rmax
		;readf, lun, zooms[i].Zmin, zooms[i].Zmax
	endfor
	print, zooms
	zoom_str = ' '
	zoom_id  = -1
	free_lun, lun
;===============================================================================
	; plot settings

	;read_plot_config, user_conf_dir+'plot.conf'
	read_plot_config, root_dir+'/plot.conf'
;===============================================================================

	base_id		= 0
	overlay_id	= 0
	fieldline_ui	= 0
	loaded_shot_fl	= ''
	tb_overlay_data	= 0L

	sim_dir	= getenv('HOME')+'/'+sim_dir
end; pro load_config
;-----------------------------------------------------------------------
