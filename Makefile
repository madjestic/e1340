.PHONY: fonts ui

all:
	cabal build && gpu ./run.sh

run:
	gpu ./run.sh

prof:
	cabal clean
	mvlink cabal.project.local cabal.project.local.prof
	mvlink e1340.cabal e1340.cabal.prof
	mvlink run.sh run.sh.prof
	cabal build

proflite:
	#cabal clean
	mvlink cabal.project.local cabal.project.local.prof
	#mvlink e1340.cabal e1340.cabal.prof
	#mvlink run.sh run.sh.prof
	cabal build

current:
	cabal clean
	mvlink cabal.project.local cabal.project.local.current
	mvlink e1340.cabal e1340.cabal.current
	mvlink run.sh run.sh.current
	cabal build
fonts:
	./resources/convertGeo.sh fnt_0
	./resources/convertGeo.sh fnt_1
	./resources/convertGeo.sh fnt_2	
	./resources/convertGeo.sh fnt_3
	./resources/convertGeo.sh fnt_4
	./resources/convertGeo.sh fnt_5
	./resources/convertGeo.sh fnt_6
	./resources/convertGeo.sh fnt_7
	./resources/convertGeo.sh fnt_8
	./resources/convertGeo.sh fnt_9
	./resources/convertGeo.sh fnt_a
	./resources/convertGeo.sh fnt_b
	./resources/convertGeo.sh fnt_c
	./resources/convertGeo.sh fnt_d
	./resources/convertGeo.sh fnt_e
	./resources/convertGeo.sh fnt_f
	./resources/convertGeo.sh fnt_g
	./resources/convertGeo.sh fnt_h
	./resources/convertGeo.sh fnt_i
	./resources/convertGeo.sh fnt_j
	./resources/convertGeo.sh fnt_k
	./resources/convertGeo.sh fnt_l
	./resources/convertGeo.sh fnt_m
	./resources/convertGeo.sh fnt_n
	./resources/convertGeo.sh fnt_o
	./resources/convertGeo.sh fnt_p
	./resources/convertGeo.sh fnt_q
	./resources/convertGeo.sh fnt_r
	./resources/convertGeo.sh fnt_s
	./resources/convertGeo.sh fnt_t
	./resources/convertGeo.sh fnt_u
	./resources/convertGeo.sh fnt_v
	./resources/convertGeo.sh fnt_w
	./resources/convertGeo.sh fnt_x
	./resources/convertGeo.sh fnt_y
	./resources/convertGeo.sh fnt_z
	./resources/convertGeo.sh fnt_plus
	./resources/convertGeo.sh fnt_minus
	./resources/convertGeo.sh fnt_equal
	./resources/convertGeo.sh fnt_GT
	./resources/convertGeo.sh fnt_LT
	./resources/convertGeo.sh fnt_comma
	./resources/convertGeo.sh fnt_dot
	./resources/convertGeo.sh fnt_question
	./resources/convertGeo.sh fnt_exclam
	./resources/convertGeo.sh fnt_asterics
	./resources/convertGeo.sh fnt_slash
	./resources/convertGeo.sh fnt_semicolon
	./resources/convertGeo.sh fnt_quote
	./resources/convertGeo.sh fnt_space
	./resources/convertGeo.sh fnt_A
	./resources/convertGeo.sh fnt_B
	./resources/convertGeo.sh fnt_C
	./resources/convertGeo.sh fnt_D
	./resources/convertGeo.sh fnt_E
	./resources/convertGeo.sh fnt_F
	./resources/convertGeo.sh fnt_G
	./resources/convertGeo.sh fnt_H
	./resources/convertGeo.sh fnt_I
	./resources/convertGeo.sh fnt_J
	./resources/convertGeo.sh fnt_K
	./resources/convertGeo.sh fnt_L
	./resources/convertGeo.sh fnt_M
	./resources/convertGeo.sh fnt_N
	./resources/convertGeo.sh fnt_O
	./resources/convertGeo.sh fnt_P
	./resources/convertGeo.sh fnt_Q
	./resources/convertGeo.sh fnt_R
	./resources/convertGeo.sh fnt_S
	./resources/convertGeo.sh fnt_T
	./resources/convertGeo.sh fnt_U
	./resources/convertGeo.sh fnt_V
	./resources/convertGeo.sh fnt_W
	./resources/convertGeo.sh fnt_X
	./resources/convertGeo.sh fnt_Y
	./resources/convertGeo.sh fnt_Z
	./resources/convertGeo.sh fnt_crosshair

ui:
	make fonts
	./resources/convertGeo.sh gizmo

intro:
	./resources/convertGeo.sh intro_square

planets:
	./resources/convertGeo.sh sun
	./resources/convertGeo.sh mercury
	./resources/convertGeo.sh venus
	./resources/convertGeo.sh earth
	./resources/convertGeo.sh moon
	./resources/convertGeo.sh mars
	./resources/convertGeo.sh jupiter

stars:
#	./resources/convertGeo.sh stars --skip
	./resources/convertGeo.sh star_sector_00 --skip
	./resources/convertGeo.sh star_sector_01 --skip
	./resources/convertGeo.sh star_sector_02 --skip
	./resources/convertGeo.sh star_sector_03 --skip
	./resources/convertGeo.sh star_sector_04 --skip
	./resources/convertGeo.sh star_sector_05 --skip
	./resources/convertGeo.sh star_sector_06 --skip
	./resources/convertGeo.sh star_sector_07 --skip
	./resources/convertGeo.sh star_sector_08 --skip
	./resources/convertGeo.sh star_sector_09 --skip

body3:
	./resources/convertGeo.sh body_0
	./resources/convertGeo.sh body_1
	./resources/convertGeo.sh body_2
	#cabal clean
	#cabal build exe:ProjectViewer
	#cabal build exe:genProject

	cabal run exe:genProject -- ./projects/body3
	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/body3

planetsputnik:
	./resources/convertGeo.sh planet_0
	./resources/convertGeo.sh sputnik
	# cabal clean
	# cabal build exe:ProjectViewer
	cabal build exe:genProject
	#  
	cabal run exe:genProject -- ./projects/planetsputnik
	cabal run exe:genUUID -- -p ./projects/planetsputnik
	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/planetsputnik

solar_system_mini:
	# cabal clean
	# cabal build exe:ProjectViewer

	./resources/convertGeo.sh star_0
	./resources/convertGeo.sh planet_0
	./resources/convertGeo.sh planet_1
	./resources/convertGeo.sh moon_0_0
	./resources/convertGeo.sh moon_0_0_0
	./resources/convertGeo.sh moon_0_0_0_0
	cabal build exe:genProject
	#  
	cabal run exe:genProject -- ./projects/solar_system_mini
	cabal run exe:genUUID -- -p ./projects/solar_system_mini
	cabal run +RTS -sstderr -N -RTS ProjectViewer ./projects/solar_system_mini
	#cabal run TS 8 ProjectViewer ./projects/solar_system_mini

solar_system_extended:
	# cabal clean
	# cabal build exe:ProjectViewer

	./resources/convertGeo.sh star_0
	./resources/convertGeo.sh planet_0
	./resources/convertGeo.sh moon_0_0
	./resources/convertGeo.sh planet_1
	./resources/convertGeo.sh moon_1_0
	./resources/convertGeo.sh planet_2
	./resources/convertGeo.sh moon_2_0
	./resources/convertGeo.sh moon_2_1

	cabal build exe:genProject
	#  
	cabal run exe:genProject -- ./projects/solar_system_extended
	cabal run exe:genUUID -- -p ./projects/solar_system_extended
	cabal run +RTS -sstderr -N -RTS ProjectViewer ./projects/solar_system_extended
	#cabal run ProjectViewer ./projects/solar_system_extended

newtest:
	cabal build exe:e1340
	cabal run exe:genProject -- ./projects/newtest
	cabal run exe:genUUID -- -p ./projects/newtest
	cabal run +RTS -sstderr -RTS e1340 ./projects/newtest ./projects/newtest

# solarsystem:
# 	./resources/convertGeo.sh sun
# 	./resources/convertGeo.sh earth
# 	cabal build exe:e1340
# 	cabal build
# 	cabal run exe:genProject -- ./projects/solarsystem
# 	cabal run exe:genUUID -- -p ./projects/solarsystem
# 	cabal run exe:genProject -- ./projects/options 	
# 	cabal run exe:genUUID -- -p ./projects/options 	
# 	cabal run exe:genProject -- ./projects/infoearth
# 	cabal run exe:genUUID -- -p ./projects/infoearth

# 	#cabal run +RTS -sstderr -RTS e1340 ./projects/solarsystem ./projects/solarsystem ./projects/options ./projects/infoearth
# 	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/solarsystem

testred:
	cabal build exe:e1340
	cabal run exe:genProject -- ./projects/testred
	cabal run exe:genUUID -- -p ./projects/testred
	cabal run +RTS -sstderr -RTS e1340 ./projects/testred ./projects/testred

testgreen:
	cabal build exe:e1340
	./resources/convertGeo.sh box_green
	cabal run exe:genProject -- ./projects/testgreen 
	cabal run exe:genUUID -- -p ./projects/testgreen 
	cabal run +RTS -sstderr -RTS e1340 ./projects/testgreen ./projects/testgreen

testblue:
	cabal build exe:e1340
	./resources/convertGeo.sh box_blue
	cabal run exe:genProject -- ./projects/testblue 
	cabal run exe:genUUID -- -p ./projects/testblue 
	cabal run +RTS -sstderr -RTS e1340 ./projects/testblue ./projects/testblue

testchecker:
	cabal build exe:e1340
	./resources/convertGeo.sh box
	cabal run exe:genProject -- ./projects/testchecker 
	cabal run exe:genUUID -- -p ./projects/testchecker 
	# cabal run +RTS -sstderr -RTS e1340 ./projects/testred ./projects/testchecker
	cabal run +RTS -sstderr -RTS e1340 ./projects/testchecker ./projects/testchecker

testcheckeroffset:
	cabal build exe:e1340
	./resources/convertGeo.sh box
	cabal run exe:genProject -- ./projects/testcheckeroffset
	cabal run exe:genUUID -- -p ./projects/testcheckeroffset
	# cabal run +RTS -sstderr -RTS e1340 ./projects/testred ./projects/testchecker
	cabal run +RTS -sstderr -RTS e1340 ./projects/testcheckeroffset ./projects/testcheckeroffset

infoearth:
	cabal build exe:genProject
	cabal run exe:genProject -- ./projects/infoearth
	cabal run exe:genUUID -- -p ./projects/infoearth
	cabal build exe:e1340
	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/infoearth

options:
	#cabal clean
	#cabal build
	#cabal build exe:genProject
	cabal run exe:genProject -- ./projects/options 	
	cabal run exe:genUUID -- -p ./projects/options 	
	#cabal build exe:e1340
	#cabal run +RTS -sstderr -RTS e1340 ./projects/options ./projects/options ./projects/options
	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/options

projectviewer:
	cabal build exe:ProjectViewer
	cabal build
	cabal run exe:genProject -- ./projects/solarsystem
	cabal run exe:genUUID -- -p ./projects/solarsystem
	#cabal run exe:genProject -- ./projects/options 	
	#cabal run exe:genUUID -- -p ./projects/options 	
	#cabal run exe:genProject -- ./projects/infoearth
	#cabal run exe:genUUID -- -p ./projects/infoearth

	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/solarsystem

curve:
	./resources/convertGeo.sh square
	cabal build exe:genProject
	#  
	cabal run exe:genProject -- ./projects/curve
	cabal run exe:genUUID -- -p ./projects/curve
	cabal run +RTS -sstderr -RTS ProjectViewer ./projects/curve	

solarsystem:
	#cabal clean
	cabal build exe:ProjectViewer

	./resources/convertGeo.sh fnt_crosshair
	# ./resources/convertGeo.sh gizmo

	#./resources/convertGeo.sh sun
	#./resources/convertGeo.sh mercury
	#./resources/convertGeo.sh venus
	#./resources/convertGeo.sh earth
	#./resources/convertGeo.sh moon
	#./resources/convertGeo.sh box
	#./resources/convertGeo.sh box_red
	#./resources/convertGeo.sh box_green
	#./resources/convertGeo.sh box_blue
	# ./resources/convertGeo.sh mars
	# ./resources/convertGeo.sh phobos
	# ./resources/convertGeo.sh deimos
	# ./resources/convertGeo.sh jupiter
	# ./resources/convertGeo.sh europa
	# ./resources/convertGeo.sh ganymede
	# ./resources/convertGeo.sh io
	# ./resources/convertGeo.sh callisto
	./resources/convertGeo.sh PNK_roll
	./resources/convertGeo.sh PNK_pitch
	./resources/convertGeo.sh PNK_yaw

	cabal build exe:genProject
	cabal build exe:genApplication
	cabal run exe:genApplication -- -a ./applications/solarsystem
	cabal run exe:genUUID -- -p ./projects/solarsystem
	cabal run exe:genUUID -- -p ./projects/test
	cabal run exe:genUUID -- -p ./projects/testred
	cabal run exe:genUUID -- -p ./projects/testgreen
	cabal run exe:genUUID -- -p ./projects/testblue
	cabal run exe:genUUID -- -p ./projects/options
	cabal run exe:genGUI
	#cabal run +RTS -sstderr -N -RTS ProjectViewer ./applications/solarsystem
	#cabal run TS 8 ProjectViewer ./projects/solar_system_mini
	cabal run exe:ProjectViewer ./applications/solarsystem

test:
	./resources/convertGeo.sh box
	cabal build exe:genProject
	cabal build exe:genApplication
	cabal run exe:genGUI
	cabal run exe:genProject     --    ./projects/test
	cabal run exe:genUUID        -- -p ./projects/test
	cabal run exe:genApplication -- -p ./projects/test
	cabal run exe:ProjectViewer ./applications/preview

box2:
	./resources/convertGeo.sh box
	./resources/convertGeo.sh box2
	cabal build exe:genApplication
	cabal build exe:genProject
	cabal run exe:genGUI
	cabal run exe:genProject     --    ./projects/box2
	cabal run exe:genUUID        -- -p ./projects/box2
	cabal run exe:genApplication -- -p ./projects/box2
	cabal run exe:ProjectViewer ./applications/preview

grapher:
	#cabal clean
	#cabal build
	./resources/convertGeo.sh graph
	cabal run exe:genProject -- ./projects/graph 	
	cabal run exe:genUUID    -- -p ./projects/graph
	cabal build exe:Grapher
	cabal run   exe:Grapher ./projects/graph ./projects/graph
	#cabal run +RTS -sstderr -RTS Grapher ./projects/graph ./projects/graph
	#cabal run +RTS -sstderr -RTS Grapher ./projects/solarsystem ./projects/solarsystem

pnk:
	./resources/convertGeo.sh box
	./resources/convertGeo.sh PNK_roll
	./resources/convertGeo.sh PNK_pitch
	./resources/convertGeo.sh PNK_yaw
	cabal build exe:genProject
	cabal build exe:genApplication
	cabal build exe:genGUI
	cabal run exe:genGUI
	cabal run exe:genApplication -- -p ./projects/pnk
	cabal run exe:genUUID        -- -p ./projects/pnk
	cabal run exe:ProjectViewer ./applications/preview
