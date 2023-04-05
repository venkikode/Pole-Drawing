(defun c:spole()
(defun DTR(x) (* (/ pi 180.0) x)) 
(defun rtd(x) (* (/ 180 pi) x))	
(initget 1 "SPD UKPD")
(setq designtype (getstring "\nEnter the Type of design:<SPD/UKPD>"))
(if (= designtype "SPD") (setq design_name (getstring "\nEnter the Design Name::")))
(if (= designtype "spd") (setq design_name (getstring "\nEnter the Design Name::")))
(setq polejoints (getint "\nEnter number of flange joints in a pole <0/1/2>:- "))
(setq typeofbolts (getstring "\nEnter the bolts types<M12/M16>::"))
(setq clampstk (getreal "\nEnter the tickness for the clamps::"))
(setq flangjointtk (getreal "\nEnter the new flange joint Tickness::"))
(setvar "cmdecho" 1)
(command "insunits" "0")
(if (= polejoints 0)
(progn
(if (= IP nil) (setq IP '(0.0 0.0 0.0)))
	(setq pedestal_width (getreal "\nEnter Pedestal Width: "))
    (setq pedestal_hight (getreal "\nEnter pedestal Hight: "))
    (setvar "osmode" 0) 
(setq pd1 (polar IP (DTR 0.0) pedestal_width)
      pd2 (polar pd1 (DTR 90.0) pedestal_hight)
	  pd3 (polar pd2 (DTR 180.0) pedestal_width)
	  pd4 (polar pd3 (DTR 270.0) pedestal_hight)
 );setq 
(setq joint_0 (getreal "\nEnter pole hight: "))
(setq diameter_1 (getreal "\nEnter first Pole diameter: "))
(setq diameter_1tick (getreal "\nEnter first Pole tickness: "))
(setq totalhight (+ joint_0 0 ))
		(setq p1 (polar pd4 (DTR 0.0) (/ pedestal_width 2))
			  p2 (polar p1 (DTR 90.0) joint_0)                       ;center line
			  p3 (polar p1 (DTR 90.0) pedestal_hight)
			  p4 (polar p3 (DTR 0.0) (/ diameter_1 2))
			  p5 (polar p4 (DTR 90.0) (- joint_0 pedestal_hight))
			  p6 (polar p5 (DTR 180.0) diameter_1)
			  p7 (polar p6 (DTR 270.0) (- joint_0 pedestal_hight))	  
        );setq
        (setq p8 (polar p2 (DTR 0.0) (/ (* diameter_1 2) 2))
			  p9 (polar p8 (DTR 270.0) 10)
			  p10 (polar p9 (DTR 180.0) (* diameter_1 2))
			  p11 (polar p10 (DTR 90.0) 10)	  
        );setq
;==================================================================================================== 
;+++++++++++++++++++++++++++++++++++++++++++++
(setq expole_pedestalhight pedestal_hight)
(setq expole_pedestalwidth pedestal_width)
(setq expolejoint1 joint_0) 
;+++++++++++++++++++++++++++++++++++++++++++++
(setq existingstud (getint "\n Enter the number of existing studs in face one<0/1/2>:"))
(setq index 0 )
(if ( and (> existingstud 0)(< existingstud 3)) 
(progn
(repeat existingstud
(setq existingstud_x (getreal "\nEnter pole center to existing struct pedestal center distance \"x\"::"))
(setq existingstud_y (getreal "\nEnter slab level to struct connected to pole distance \"y\"::"))
(setq existingstud_pedestalwidth (getreal "\nEnter the Existing struct pedestal width::"))
(setq existingstud_pedestalhight (getreal "\nEnter the Existing strcat pedestal hight::"))
(setq twost_ang (getreal "\nEnter the angle between the two existing structs in two faces in plan::")) ;for plan and hip

(setq p12 (polar p1 (DTR 0.0) existingstud_x)) ; pedestal center point
(setq p13 (polar p12 (DTR 90.0) 75)) ;member hole center point
(setq p14 (polar p1 (DTR 90.0) existingstud_y)) ;member clamped location on pole center
(setq p15 (polar p14 (DTR 0.0) (/ diameter_1 2)))
(setq p16 (polar p15 (DTR 90.0) 50))
(setq p17 (polar p16 (DTR 0.0) 100))
(setq p18 (polar p17 (DTR 270.0) 100))
(setq p19 (polar p18 (DTR 180.0) 100))
(setq p20 (polar p15 (DTR 0.0) 75))   ;hole location

(setq p21 (polar p12 (DTR 0.0) (/ existingstud_pedestalwidth 2)))
(setq p22 (polar p21 (DTR 90.0) existingstud_pedestalhight ))
(setq p23 (polar p22 (DTR 180.0) existingstud_pedestalwidth ))
(setq p24 (polar p23 (DTR 270.0) existingstud_pedestalhight ))

(setq pp1 p20)
(setq pp2 p13)
 
(initget 1 "A P")
(setq existingstud_membertype (getkword "\nEnter the extsting struct member type <Angle/Pipe>::"))
(setq existingstud_membersize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft existingstud_membersize)
(if (= existingstud_membertype "A") (angmember))
(if (= existingstud_membertype "P") (pipemember))
;(command "line" p16 p17 p18 p19 "")
;(command "_insert" "16c" p20 "" "" "")
;(command "line" p20 p13 "")
;(command "line" p21 p22 p23 p24 p21 "") 
(setq index (1+ index))
;+++++++++++++++++++++++++++++++++++++
(if (= index 1) (progn (setq exst1_x existingstud_x) (setq exst1_y existingstud_y) (setq exst1_pw existingstud_pedestalwidth) (setq exst1_ph existingstud_pedestalhight) (setq exst1_ty existingstud_membertype) (setq exst1_name memname)))
(if (= index 2) (progn (setq exst2_x existingstud_x) (setq exst2_y existingstud_y) (setq exst2_pw existingstud_pedestalwidth) (setq exst2_ph existingstud_pedestalhight) (setq exst2_ty existingstud_membertype) (setq exst2_name memname)))
;+++++++++++++++++++++++++++++++++++++
;;;existing members on existing struct 
(setq polestarting p1)
(setq poleending p2)
(setq structstarting p20)
(setq structending p13)
(setq structsize existingstud_membersize)

(existing_newm)
(hip_eqfaces)
)));existing member end
(setq polestarting p1)
(setq poleending p2)
(newstud)
(spd_drawings)
)) ;joint_0 end
(if (= polejoints 1)
(progn
(if (= IP nil) (setq IP '(0.0 0.0 0.0)))
	(setq pedestal_width (getreal "\nEnter Pedestal Width: "))
    (setq pedestal_hight (getreal "\nEnter pedestal Hight: "))
    (setvar "osmode" 0) 
(setq pd1 (polar IP (DTR 0.0) pedestal_width)
      pd2 (polar pd1 (DTR 90.0) pedestal_hight)
	  pd3 (polar pd2 (DTR 180.0) pedestal_width)
	  pd4 (polar pd3 (DTR 270.0) pedestal_hight)
	  
 );setq
 ;(command "LINE" IP pd1 pd2 pd3 pd4 "" )
 (setq joint_0 (getreal "\nEnter pole hight: "))
 (setq joint_1 (getreal "\nEnter joint_1 hight from slab level: "))
 (setq diameter_1 (getreal "\nEnter first Pole diameter: "))
(setq diameter_1tick (getreal "\nEnter first Pole tickness: "))
 (setq diameter_2 (getreal "\nEnter second Pole diameter: "))
(setq diameter_2tick (getreal "\nEnter Secound Pole tickness: "))
 (setq totalhight (+ joint_0 0 ))
 
		(setq p1 (polar pd4 (DTR 0.0) (/ pedestal_width 2))
			  p2 (polar p1 (DTR 90.0) totalhight)                       ;center line
			  p3 (polar p1 (DTR 90.0) pedestal_hight)
			  p4 (polar p3 (DTR 0.0) (/ diameter_1 2))
			  p5 (polar p4 (DTR 90.0) (- joint_1 pedestal_hight))
			  p6 (polar p5 (DTR 180.0) diameter_1)
			  p7 (polar p6 (DTR 270.0) (- joint_1 pedestal_hight))
			  
			  p8 (polar p1 (DTR 90.0) joint_1)  ;joint_1 hight point on pole center line.
			  p9 (polar p8 (DTR 0.0) (/ diameter_2 2))
			  p10 (polar p9 (DTR 90.0) (- totalhight joint_1))
			  p11 (polar p10 (DTR 180.0) diameter_2)
			  p12 (polar p11 (DTR 270.0) (- totalhight joint_1))				
        );setq
        (setq p13 (polar p8 (DTR 0.0) (/ (* diameter_1 2) 2))
			  p14 (polar p13 (DTR 270.0) 10)
			  p15 (polar p14 (DTR 180.0) (* diameter_1 2))
			  p16 (polar p15 (DTR 90.0) 20)	  
			  p17 (polar p16 (DTR 0.0) (* diameter_1 2))
			  p18 (polar p16 (DTR 270.0) 10)     ;for flang center line
				) ;setq 
        (setq p20 (polar p2 (DTR 0.0) (/ (* diameter_1 2) 2))
			  p21 (polar p20 (DTR 270.0) 10)
			  p22 (polar p21 (DTR 180.0) (* diameter_1 2))
			  p23 (polar p22 (DTR 90.0) 10)	  
        );setq
 ;(command "LINE" p2 p3 "" ) ;center line
 ;(command "LINE" p4 p5 p6 p7 "" "line" p9 p10 p11 p12 "" )
 ;(command "LINE" p14 p15 p16 p17 p14 "" "line" p13 p18 "" )
 ;(command "line" p20 p21 p22 p23 p20 "")
 ;(command "_zoom" IP p2)
  ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq expole_pedestalhight pedestal_hight)
(setq expole_pedestalwidth pedestal_width)
(setq expolejoint1 joint_1) 
(setq expolejoint_end (- totalhight joint_1))
 ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq existingstud (getint "\n Enter the number of existing studs in face one<0/1/2>:"))
(setq index 0)
(if ( and (> existingstud 0)(< existingstud 3)) 
(progn
(repeat existingstud
(setq existingstud_x (getreal "\nEnter pole center to existing pedestal center distance \"x\"::"))
(setq existingstud_y (getreal "\nEnter slab level to struct connected to pole distance \"y\"::"))
(setq existingstud_pedestalwidth (getreal "\nEnter the Existing struct pedestal width::"))
(setq existingstud_pedestalhight (getreal "\nEnter the Existing strcat pedestal hight::"))
(setq twost_ang (getreal "\nEnter the angle between the two existing structs in two faces in plan::")) ;for plan and hip

(setq p24 (polar p1 (DTR 0.0) existingstud_x)) ; pedestal center point
(setq p25 (polar p24 (DTR 90.0) 75)) ;member hole center point at pedestal

(if (< existingstud_y joint_1) (setq diameter diameter_1) )
(if (> existingstud_y joint_1) (setq diameter diameter_2) )

(setq p26 (polar p1 (DTR 90.0) existingstud_y)) ;member clamped location on pole center
(setq p27 (polar p26 (DTR 0.0) (/ diameter 2)))
(setq p28 (polar p27 (DTR 90.0) 50))
(setq p29 (polar p28 (DTR 0.0) 100))
(setq p30 (polar p29 (DTR 270.0) 100))
(setq p31 (polar p30 (DTR 180.0) 100))
(setq p32 (polar p27 (DTR 0.0) 75))   ;hole location

(setq p33 (polar p24 (DTR 0.0) (/ existingstud_pedestalwidth 2)))
(setq p34 (polar p33 (DTR 90.0) existingstud_pedestalhight ))
(setq p35 (polar p34 (DTR 180.0) existingstud_pedestalwidth ))
(setq p36 (polar p35 (DTR 270.0) existingstud_pedestalhight ))

;(command "line" p28 p29 p30 p31 p28 "")
;(command "_insert" "16c" p32 "" "" "")
;(command "line" p32 p25 "")

(setq pp1 p32)
(setq pp2 p25)
 
(initget 1 "A P")
(setq existingstud_membertype (getkword "\nEnter the extsting struct member type <Angle/Pipe>::"))
(setq existingstud_membersize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft existingstud_membersize)
(if (= existingstud_membertype "A") (angmember))
(if (= existingstud_membertype "P") (pipemember))
;(command "LINE" p33 p34 p35 p36 p33 "" )
(setq index (1+ index))
;+++++++++++++++++++++++++++++++++++++
(if (= index 1) (progn (setq exst1_x existingstud_x) (setq exst1_y existingstud_y) (setq exst1_pw existingstud_pedestalwidth) (setq exst1_ph existingstud_pedestalhight) (setq exst1_name memname)))
(if (= index 2) (progn (setq exst2_x existingstud_x) (setq exst2_y existingstud_y) (setq exst2_pw existingstud_pedestalwidth) (setq exst2_ph existingstud_pedestalhight) (setq exst2_name memname)))
;+++++++++++++++++++++++++++++++++++++ 
;existing member
(setq polestarting p1)
(setq poleending p2)
(setq structstarting p32)
(setq structending p25)
(setq structsize existingstud_membersize)
(existing_newm)
(hip_eqfaces)
)
));existing member end
(setq polestarting p1)
(setq poleending p2)
(newstud)
(spd_drawings)
)) ;joint_1 end
(if (= polejoints 2)
(progn
(if (= IP nil) (setq IP '(0.0 0.0 0.0)))
	(setq pedestal_width (getreal "\nEnter Pedestal Width: "))
    (setq pedestal_hight (getreal "\nEnter pedestal Hight: "))
    (setvar "osmode" 0) 
(setq pd1 (polar IP (DTR 0.0) pedestal_width)
      pd2 (polar pd1 (DTR 90.0) pedestal_hight)
	  pd3 (polar pd2 (DTR 180.0) pedestal_width)
	  pd4 (polar pd3 (DTR 270.0) pedestal_hight)
 );setq
 ;(command "LINE" IP pd1 pd2 pd3 pd4 "" )
 
 (setq joint_0 (getreal "\nEnter pole hight: "))
 (setq joint_1 (getreal "\nEnter joint_1 hight from slab level: "))
 (setq joint_2 (getreal "\nEnter joint_2 hight from slab level: "))
 
 (setq diameter_1 (getreal "\nEnter first Pole diameter: "))
 (setq diameter_1tick (getreal "\nEnter first Pole tickness: "))
 (setq diameter_2 (getreal "\nEnter second Pole diameter: "))
 (setq diameter_2tick (getreal "\nEnter second Pole tickness: "))
 (setq diameter_3 (getreal "\nEnter thired Pole diameter: "))
 (setq diameter_3tick (getreal "\nEnter thired Pole tickness: "))
 (setq totalhight (+ joint_0 0 ))
 
		(setq p1 (polar pd4 (DTR 0.0) (/ pedestal_width 2))
			  p2 (polar p1 (DTR 90.0) totalhight)                       ;center line
			  p3 (polar p1 (DTR 90.0) pedestal_hight) 
			  p4 (polar p3 (DTR 0.0) (/ diameter_1 2))
			  p5 (polar p4 (DTR 90.0) (- joint_1 pedestal_hight))
			  p6 (polar p5 (DTR 180.0) diameter_1)
			  p7 (polar p6 (DTR 270.0) (- joint_1 pedestal_hight))
			  p8 (polar p1 (DTR 90.0) joint_1)  ;joint_1 hight point on pole center line.
			  p9 (polar p8 (DTR 0.0) (/ diameter_2 2))
			  p10 (polar p9 (DTR 90.0) (- joint_2 joint_1))
			  p11 (polar p10 (DTR 180.0) diameter_2)
			  p12 (polar p11 (DTR 270.0) (- joint_2 joint_1))
			  p13 (polar p1 (DTR 90.0) joint_2)  ;joint_2 hight point on pole center line.
			  p14 (polar p13 (DTR 0.0) (/ diameter_3 2))
			  p15 (polar p14 (DTR 90.0) (- totalhight joint_2))
			  p16 (polar p15 (DTR 180.0) diameter_3)
			  p17 (polar p16 (DTR 270.0) (- totalhight joint_2))			  
        );setq
 ;===joint_1 flang's====;
        (setq p18 (polar p8 (DTR 0.0) (/ (* diameter_1 2) 2))
			  p19 (polar p18 (DTR 270.0) 10)
			  p20 (polar p19 (DTR 180.0) (* diameter_1 2))
			  p21 (polar p20 (DTR 90.0) 20)	  
			  p22 (polar p21 (DTR 0.0) (* diameter_1 2))
			  p23 (polar p21 (DTR 270.0) 10)     ;for flang center line
				) ;setq 
 ;===joint_2 flang's====;
        (setq p24 (polar p13 (DTR 0.0) (/ (* diameter_1 2) 2))
			  p25 (polar p24 (DTR 270.0) 10)
			  p26 (polar p25 (DTR 180.0) (* diameter_1 2))
			  p27 (polar p26 (DTR 90.0) 20)	  
			  p28 (polar p27 (DTR 0.0) (* diameter_1 2))
			  p29 (polar p27 (DTR 270.0) 10)     ;for flang center line
				) ;setq 
 ;===end flang's====;
        (setq p30 (polar p2 (DTR 0.0) (/ (* diameter_1 2) 2))
			  p31 (polar p30 (DTR 270.0) 10)
			  p32 (polar p31 (DTR 180.0) (* diameter_1 2))
			  p33 (polar p32 (DTR 90.0) 10)	  
        );setq
 
 ;(command "LINE" p2 p3 "" ) ;center line
 ;(command "LINE" p4 p5 p6 p7 "" "line" p9 p10 p11 p12 "" "line" p14 p15 p16 p17 "" "LINE" p19 p20 p21 p22 p19 "" "line" p18 p23 "" "LINE" p25 p26 p27 p28 p25 "" "line" p24 p29 "")
 ;(command "line" p30 p31 p32 p33 p30 "" "_zoom" IP p2)
  
 ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq expole_pedestalhight pedestal_hight)
(setq expole_pedestalwidth pedestal_width)
(setq expolejoint1 joint_1) 
(setq expolejoint2 (- joint_2 joint_1)) 
(setq expolejoint_end (- totalhight joint_2))
 ;+++++++++++++++++++++++++++++++++++++++++++++++++++++++ 

(setq existingstud (getint "\n Enter the number of existing studs in face one<0/1/2>:"))
(setq index 0)
(if ( and (> existingstud 0)(< existingstud 3)) 
(progn
(repeat existingstud
(setq existingstud_x (getreal "\nEnter pole center to existing pedestal center distance \"x\"::"))
(setq existingstud_y (getreal "\nEnter slab level to struct connected to pole distance \"y\"::"))
(setq existingstud_pedestalwidth (getreal "\nEnter the Existing struct pedestal width::"))
(setq existingstud_pedestalhight (getreal "\nEnter the Existing strcat pedestal hight::"))
(setq twost_ang (getreal "\nEnter the angle between the two existing structs in two faces in plan::")) ;for plan and hip

(setq p34 (polar p1 (DTR 0.0) existingstud_x)) ; pedestal center point
(setq p35 (polar p34 (DTR 90.0) 75)) ;member hole center point at pedestal

(if (< existingstud_y joint_1) (setq diameter diameter_1) )
(if ( and (> existingstud_y joint_1) (< existingstud_y joint_2)) (setq diameter diameter_2) )
(if (> existingstud_y joint_2) (setq diameter diameter_3) )

(setq p36 (polar p1 (DTR 90.0) existingstud_y)) ;member clamped location on pole center
(setq p37 (polar p36 (DTR 0.0) (/ diameter 2)))
(setq p38 (polar p37 (DTR 90.0) 50))
(setq p39 (polar p38 (DTR 0.0) 100))
(setq p40 (polar p39 (DTR 270.0) 100))
(setq p41 (polar p40 (DTR 180.0) 100))
(setq p42 (polar p37 (DTR 0.0) 75))   ;hole location
(setq p43 (polar p34 (DTR 0.0) (/ existingstud_pedestalwidth 2)))
(setq p44 (polar p43 (DTR 90.0) existingstud_pedestalhight ))
(setq p45 (polar p44 (DTR 180.0) existingstud_pedestalwidth ))
(setq p46 (polar p45 (DTR 270.0) existingstud_pedestalhight ))

(setq pp1 p42)
(setq pp2 p35)
 
(initget 1 "A P")
(setq existingstud_membertype (getkword "\nEnter the extsting struct member type <Angle/Pipe>::"))
(setq existingstud_membersize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft existingstud_membersize)
(if (= existingstud_membertype "A") (angmember))
(if (= existingstud_membertype "P") (pipemember))
;(command "line" p38 p39 p40 p41 p38 "")
;(command "_insert" "16c" p42 "" "" "")
;(command "line" p42 p35 "")
;(command "LTSCALE" 1)
;(command "LINE" p43 p44 p45 p46 p43 "" )
(setq index (1+ index)) 
;+++++++++++++++++++++++++++++++++++++
(if (= index 1) (progn (setq exst1_x existingstud_x) (setq exst1_y existingstud_y) (setq exst1_pw existingstud_pedestalwidth) (setq exst1_ph existingstud_pedestalhight) (setq exst1_name memname)))
(if (= index 2) (progn (setq exst2_x existingstud_x) (setq exst2_y existingstud_y) (setq exst2_pw existingstud_pedestalwidth) (setq exst2_ph existingstud_pedestalhight) (setq exst2_name memname)))
;+++++++++++++++++++++++++++++++++++++
(setq polestarting p1)
(setq poleending p2)
(setq structstarting p42)
(setq structending p35)
(setq structsize existingstud_membersize)
(existing_newm)
(hip_eqfaces)

)));existing member end
(setq polestarting p1)
(setq poleending p2)
(newstud)
(spd_drawings)
;(command "layer" "s" "mem" "")
)) ;joint_2 end 
)   ;main program end
(defun newstud()
(setq newstudnum (getint "\n Enter the number of new struct in face one<0/1/2>:"))
(setq index 0)
(if ( and (> newstudnum 0)(< newstudnum 3)) 
(progn
(repeat newstudnum
(setq newstud_x (getreal "\nEnter pole center to new pedestal center distance \"x\"::"))
(setq newstud_y (getreal "\nEnter slab level to struct connected to pole distance \"y\"::"))
(setq newstud_pedestalwidth (getreal "\nEnter the new struct pedestal width::"))
(setq newstud_pedestalhight (getreal "\nEnter the new strcat pedestal hight::"))
(setq newtwost_ang (getreal "\nEnter the angle between the two existing structs in two faces in plan::")) ;for plan and hip
(setq bptk (getreal "\nEnter the tickness for the baseplate")) ;baseplate tickness

(setq ns1 (polar p1 (DTR 0.0) newstud_x)) ; pedestal center point
(setq ns2 (polar p1 (DTR 90.0) newstud_y)) ;member clamped location on pole center
(setq nsp1 (polar ns1 (DTR 0.0) (/ newstud_pedestalwidth 2)))
(setq nsp2 (polar nsp1 (DTR 90.0) newstud_pedestalhight ))
(setq nsp3 (polar nsp2 (DTR 180.0) newstud_pedestalwidth ))
(setq nsp4 (polar nsp3 (DTR 270.0) newstud_pedestalhight ))
;(command "line" nsp1 nsp2 nsp3 nsp4 nsp1 "")
(if (= polejoints 0)(setq diameter diameter_1))
(if (= polejoints 1)(progn (if (< newstud_y joint_1) (setq diameter diameter_1)) (if (> newstud_y joint_1) (setq diameter diameter_2))))
(if (= polejoints 2)(progn (if (< newstud_y joint_1) (setq diameter diameter_1)) (if ( and (> newstud_y joint_1) (< newstud_y joint_2)) (setq diameter diameter_2)) (if (> newstud_y joint_2) (setq diameter diameter_3))))

(initget 1 "A P")
(setq newstud_membertype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newstud_membersize (getstring "\nEnter the new struct member size::"))
(setq memshoft newstud_membersize)

(if (= newstud_membertype "A")
(progn 
(setq ccin ns2) (setq pipedia diameter) 
(C-Clamp2)
(setq acin ns1)
(GA-01_twobolts)
(setq n_ang1 (angle c11 ac11))
(setq n_ang2 (angle ac11 c11))
(setq c12 (polar c11 n_ang2 40))
;(command "_insert" "16c" c12 "" "" "")
(setq ac12 (polar ac11 n_ang1 40))
;(command "_insert" "16c" ac12 "" "" "")
(setq structending ac12)
(setq structstarting c12)
(setq pp1 c12)
(setq pp2 ac12)
;(command "line" c12 ac12 "")
(angmember)
))
(if (= newstud_membertype "P")
(progn
(setq ccin ns2) (setq pipedia diameter) 
(C-Clamp2)
(setq acin ns1)
(GA-01_twobolts)
(setq n_ang1 (angle c11 ac11))
(setq n_ang2 (angle ac11 c11))
(setq c12 (polar c11 n_ang2 40))
;(command "_insert" "16c" c12 "" "" "")
(setq ac12 (polar ac11 n_ang1 40))
;(command "_insert" "16c" ac12 "" "" "")
(setq pp1 c12)
(setq pp2 ac12)
(setq structending ac12)  
(setq structstarting c12)
;(command "line" ac12 c12 "") newst
(pipemember_twobolts)))

(setq newst_ang (- 180 (rtd n_ang2)))
(setq newst_platex1 (abs (Number_Round (* 40 (sin newst_ang)) 1)))
(setq newst_platey1 (Number_Round (sqrt (- (* 40 40) (* newst_platex1 newst_platex1))) 1))
(setq newst_platex2 (- 150 (+ newst_platex1 25)))
(setq newst_platey2 (- 100 (+ newst_platey1 25)))

;************************************************************
(setq index (1+ index))
(if (= index 1) (progn 
(setq newstud1_x1 newstud_x) (setq newstud1_y1 newstud_y)
(setq newstud1_pdwt newstud_pedestalwidth) (setq newstud1_pdht newstud_pedestalhight)
(setq newstud1_memty newstud_membertype)
(setq newstud1_memlen (distance pp1 pp2))
(setq newstud1_memname memname)
(setq newstud1_blockwt blockwt)
(setq newstud1_backmark bm)
(setq newstud1_platex1 newst_platex1) ;baseplate cleat
(setq newstud1_platey1 newst_platey1) ;baseplate cleat
(setq newstud1_platex2 newst_platex2) ;baseplate cleat
(setq newstud1_platey2 newst_platey2) ;baseplate cleat
(setq newstud1_clampx1 cc2_d1) ;struct clamp plate upprer dim
(setq newstud1_clmapx2 cc2_d2) ;struct clamp pltte lower dim
(if (= newstud_membertype "P") (progn (setq newst1_sizename1 DD3) (setq newst1_sizename2 L2) (setq newst1_size DD1) (setq newst1_thick DDTICK)))
(if (= newstud_membertype "A") (progn  (setq newst1_size flange) (setq newst1_thick th)))
(setq structsize newst1_size) ;for hip
))
  
(if (= index 2) (progn 
(setq newstud2_x1 newstud_x) (setq newstud2_y1 newstud_y)
(setq newstud2_pdwt newstud_pedestalwidth) (setq newstud2_pdht newstud_pedestalhight)
(setq newstud2_memty newstud_membertype)
(setq newstud2_memlen (distance pp1 pp2))
(setq newstud2_memname memname)
(setq newstud2_blockwt blockwt)
(setq newstud2_backmark bm)
(setq newstud2_platex1 newst_platex1) ;baseplate cleat
(setq newstud2_platey1 newst_platey1) ;baseplate cleat
(setq newstud2_platex2 newst_platex2) ;baseplate cleat
(setq newstud2_platey2 newst_platey2) ;baseplate cleat
(setq newstud2_clampx1 cc2_d1) ;struct clamp plate upprer dim
(setq newstud2_clmapx2 cc2_d2) ;struct clamp pltte lower dim
(if (= newstud_membertype "P") (progn (setq newst2_sizename1 DD3) (setq newst2_sizename2 L2) (setq newst2_size DD1) (setq newst2_thick DDTICK)))
(if (= newstud_membertype "A") (progn  (setq newst2_size flange) (setq newst2_thick th)))
(setq structsize newst2_size) ;for hip
))
;************************************************************
(setq twost_ang newtwost_ang)
(setq exst1_x newstud_x)
(setq exst1_ty newstud_membertype)
(hip_eqfaces)
;************HIP TRIGGERS*************
(if (= hip_mem 1) (progn
(if (= index 1) (progn (setq newstud1_hipmem1_len hip_memlen1) (setq newstud1_hipmem1_type hip_mem1_type) 
(setq newstud1_hipmem1_ang1 hip_plan_ang1) (setq newstud1_hipmem1_ang2 hip_plan_ang2)
(setq newstud1_hipmem1_memname memname)
(setq newstud1_hipmem1_blockwt blockwt)
(setq newstud1_hipmem1_backmark bm)
(if (= hip_mem1_type "P") (progn (setq newstud1_hipmem1_sizename1 DD3) (setq newstud1_hipmem1_sizename2 L2) (setq newstud1_hipmem1_size DD1) (setq newstud1_hipmem1_thick DDTICK) ))
(if (= hip_mem1_type "A") (progn (setq newstud1_hipmem1_size flange) (setq newstud1_hipmem1_thick th) ))
))
(if (= index 2) (progn (setq newstud2_hipmem1_len hip_memlen1) (setq newstud2_hipmem1_type hip_mem1_type) 
(setq newstud2_hipmem1_ang1 hip_plan_ang1) (setq newstud2_hipmem1_ang2 hip_plan_ang2)
(setq newstud2_hipmem1_memname memname)
(setq newstud2_hipmem1_blockwt blockwt)
(setq newstud2_hipmem1_backmark bm)
(if (= hip_mem1_type "P") (progn (setq newstud2_hipmem1_sizename1 DD3) (setq newstud2_hipmem1_sizename2 L2) (setq newstud2_hipmem1_size DD1) (setq newstud2_hipmem1_thick DDTICK) ))
(if (= hip_mem1_type "A") (progn (setq newstud2_hipmem1_size flange) (setq newstud2_hipmem1_thick th)))
))
))

(if (= hip_mem 2) (progn
(if (= index 1) (progn (setq newstud1_hipmem1_len hip_memlen1) (setq newstud1_hipmem1_type hip_mem1_type) (setq newstud1_hipmem1_ang1 hip_plan_ang1) (setq newstud1_hipmem1_ang2 hip_plan_ang2)
					   (setq newstud1_hipmem2_len hip_memlen2) (setq newstud1_hipmem2_type hip_mem1_type) (setq newstud1_hipmem2_ang1 hip_plan_ang1) (setq newstud1_hipmem2_ang2 hip_plan_ang2)
(setq newstud1_hipmem1_memname memname)
(setq newstud1_hipmem1_blockwt blockwt)
(setq newstud1_hipmem1_backmark bm)
(if (= hip_mem1_type "P") (progn (setq newstud1_hipmem1_sizename1 DD3) (setq newstud1_hipmem1_sizename2 L2) (setq newstud1_hipmem1_size DD1) (setq newstud1_hipmem1_thick DDTICK) ))
(if (= hip_mem1_type "A") (progn (setq newstud1_hipmem1_size flange) (setq newstud1_hipmem1_thick th) ))
					   ))
(if (= index 2) (progn (setq newstud2_hipmem1_len hip_memlen1) (setq newstud2_hipmem1_type hip_mem1_type) (setq newstud2_hipmem1_ang1 hip_plan_ang1) (setq newstud2_hipmem1_ang2 hip_plan_ang2)
					   (setq newstud2_hipmem2_len hip_memlen2) (setq newstud2_hipmem2_type hip_mem1_type) (setq newstud2_hipmem2_ang1 hip_plan_ang1) (setq newstud2_hipmem2_ang2 hip_plan_ang2)
(setq newstud2_hipmem1_memname memname)
(setq newstud2_hipmem1_blockwt blockwt)
(setq newstud2_hipmem1_backmark bm)
(if (= hip_mem1_type "P") (progn (setq newstud2_hipmem1_sizename1 DD3) (setq newstud2_hipmem1_sizename2 L2) (setq newstud2_hipmem2_size DD1) (setq newstud2_hipmem1_thick DDTICK) ))
(if (= hip_mem1_type "A") (progn (setq newstud2_hipmem2_size flange) (setq newstud2_hipmem1_thick th)))
					   ))
					   ))
;*************************************
(setq newst_mem_onpole (getint "\nEnter the number of new members on pole<0/1/2>:"))
(setq newst_mem_onstruct (getint "\nEnter the number of new members on struct<0/1/2>:"))
(if (and (= newst_mem_onpole 1) (= newst_mem_onstruct 1))
(progn
(setq new_mem_y1 (getint "\nEnter the vertical distance from slab to new member on pole:"))
(setq new_mem_y2 (getint "\nEnter the vertical distance from slab to new member on struct:"))

(setq newm1 (polar polestarting (DTR 90.0) new_mem_y1)) ;point for new member on pole
(setq newm2 (polar polestarting (DTR 90.0) new_mem_y2)) ;referance point for new member on struct
(setq newm3 (polar newm2 (DTR 0.0) 10000))
(setq newm4 (inters structending structstarting newm2 newm3)) ;point for new member on struct

(if (= polejoints 0) (setq diameter diameter_1))
(if (= polejoints 1) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if (> new_mem_y1 joint_1) (setq diameter diameter_2))))
(if (= polejoints 2) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if ( and (> new_mem_y1 joint_1) (< new_mem_y1 joint_2)) (setq diameter diameter_2)) (if (> new_mem_y1 joint_2) (setq diameter diameter_3))))

(setq ccin newm1) 
(setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location

(setq newm_ang1 (angle structstarting structending))
(setq newm_ang2 (rtd newm_ang1))
(setq newm_ang3 (- 180 newm_ang2))
(setq newm_ang4 (DTR newm_ang3))

(if (= newstud_membertype "A") 
(progn 
;(command "_insert" "16c" newm4 "" "" "") 
(setq pp2 newm4) 
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))))

(if (= newstud_membertype "P") 
(progn (setq newm11 (/ structsize 2)) (setq newm12 (/ newm11 (sin newm_ang4)))
(setq tempcal (sqrt (- (* (/ 80 (sin newm_ang4)) (/ 80 (sin newm_ang4))) (* 80 80) )))
(setq newm13 (polar newm4 (DTR 0.0) newm12))
(setq newm14 (polar newm13 newm_ang1 (/ 40 (sin newm_ang4))))
(setq newm15 (polar newm14 (DTR 180.0) 75))
(setq newm16 (polar newm15 (DTR 270.0) 80))
(setq newm17 (polar newm16 (DTR 0.0) (+ tempcal 75)))
(setq newm18ttt (polar newm14 (DTR 180.0) 50))
(setq newm18 (polar newm18ttt (DTR 270.0) 40)) ;hole location on struct
;(command "line" newm14 newm15 newm16 newm17 "")
;(command "_insert" "16c" newm18 "" "" "")
(setq pp2 newm18)
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))))
;********************************************************
(setq newst1_newmem1_y1 new_mem_y1) (setq newst1_newmem1_y2 new_mem_y2) 
(setq newst1_newmem1_type newm_memtype)  
(setq newst1_newmem1_len (Number_Round (distance pp1 pp2) 1))
(setq newst1_newmem1_memname memname)
(setq newst1_newmem1_blockwt blockwt)
(setq newst1_newmem1_backmark bm)
(if (= newm_memtype "P") (progn (setq newst1_newmem1_sizename1 DD3) (setq newst1_newmem1_sizename2 L2) (setq newst1_newmem1_size DD1) (setq newst1_newmem1_thick DDTICK) 
							(setq newst1_newmem1_stx1 (Number_Round (+ tempcal 100) 1)) ;struct plate for connect new member
							(setq newst1_newmem1_stx2 (Number_Round tempcal 1)) ;struct plate for connect new member
						))
(if (= newm_memtype "A") (progn  (setq newst1_newmem1_size flange) (setq newst1_newmem1_thick th)))
(setq newst1_newmem1_pclampx1 cc1_d1) 
(setq newst1_newmem1_pclampx2 cc1_d2) 
(setq newst1_newmem1_pclampR R_cc1)

(if (= newstud_membertype "P") (progn 
	(if (> (distance structstarting structending) 6000) (progn
		(setq newst1_joint1 (Number_Round (+ (distance structstarting newm4) 100) 1)) ;100(plate clear)
		(setq newst1_joint2 (Number_Round (- (distance newm4 structending) 100) 1))
	))
	(if (< (distance structstarting structending) 6000) (progn
		(setq newst1_joint1 (Number_Round (distance structstarting newm4) 1)) ;no need of 100(plate clear)+25(shearedge)
		(setq newst1_joint2 (Number_Round (distance newm4 structending) 1))
	))
))
(if (= newstud_membertype "A") (progn 
	(if (> (distance structstarting structending) 6000) (progn
		(setq newst1_joint1 (Number_Round (+ (distance structstarting newm4) 200) 1)) ;100(plate clear)
		(setq newst1_joint2 (Number_Round (- (distance newm4 structending) 200) 1))
	))
	(if (< (distance structstarting structending) 6000) (progn
		(setq newst1_joint1 (Number_Round (distance structstarting newm4) 1)) ;no need of joint 100(plate clear)+25(shearedge)
		(setq newst1_joint2 (Number_Round (distance newm4 structending) 1))
	))
))

;********************************************************
));opt1,opt1 newst1_joint2
(if (and (= newst_mem_onpole 1) (= newst_mem_onstruct 2))
(progn
(setq new_mem_y1 (getint "\nEnter the vertical distance from slab to new member on pole:"))
(setq new_mem_y2 (getint "\nEnter the vertical distance from slab to bollom (or first ) new member on struct:"))
(setq new_mem_y3 (getint "\nEnter the vertical distance from slab to top (or secound) new member on struct:"))

(setq newm1 (polar polestarting (DTR 90.0) new_mem_y1)) ;point for new member on pole
(setq newm2 (polar polestarting (DTR 90.0) new_mem_y2)) 
(setq newm3 (polar newm2 (DTR 0.0) 10000))
(setq newm4 (inters structending structstarting newm2 newm3)) ;first point for new member on struct
(setq newm5 (polar polestarting (DTR 90.0) new_mem_y3))
(setq newm6 (polar newm5 (DTR 0.0) 10000))
(setq newm7 (inters structending structstarting newm5 newm6)) ;second point for new member on struct

(if (= polejoints 0) (setq diameter diameter_1)) 
(if (= polejoints 1) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if (> new_mem_y1 joint_1) (setq diameter diameter_2))))
(if (= polejoints 2) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if ( and (> new_mem_y1 joint_1) (< new_mem_y1 joint_2)) (setq diameter diameter_2)) (if (> new_mem_y1 joint_2) (setq diameter diameter_3))))

(setq ccin newm1) 
(setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq newm_ang1 (angle structstarting structending))
(setq newm_ang2 (rtd newm_ang1))
(setq newm_ang3 (- 180 newm_ang2))
(setq newm_ang4 (DTR newm_ang3))

(if (= newstud_membertype "A") 
(progn 
;(command "_insert" "16c" newm4 "" "" "")
(setq pp2 newm4) 
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new struct member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))
;(command "_insert" "16c" newm7 "" "" "")
(setq pp2 newm7) 
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new struct member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))
))

(if (= newstud_membertype "P") 
(progn 
(setq newm11 (/ newstud_membersize 2)) (setq newm12 (/ newm11 (sin newm_ang4)))
(setq tempcal (sqrt (- (* (/ 80 (sin newm_ang4)) (/ 80 (sin newm_ang4))) (* 80 80) )))

(setq newm13 (polar newm4 (DTR 0.0) newm12))
(setq newm14 (polar newm13 newm_ang1 (/ 40 (sin newm_ang4))))
(setq newm15 (polar newm14 (DTR 180.0) 75))
(setq newm16 (polar newm15 (DTR 270.0) 80))
(setq newm17 (polar newm16 (DTR 0.0) (+ tempcal 75)))
(setq newm18ttt (polar newm14 (DTR 180.0) 50))
(setq newm18 (polar newm18ttt (DTR 270.0) 40)) ;hole location on struct

;(command "line" newm14 newm15 newm16 newm17 "")
;(command "_insert" "16c" newm18 "" "" "")
(setq pp2 newm18)
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new struct member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))
;;
(setq newm19 (polar newm7 (DTR 0.0) newm12))
(setq newm20 (polar newm19 newm_ang1 (/ 40 (sin newm_ang4))))
(setq newm21 (polar newm20 (DTR 180.0) 100))
(setq newm22 (polar newm21 (DTR 270.0) 80))
(setq newm23 (polar newm22 (DTR 0.0) (+ tempcal 100)))
(setq newm24 (polar newm19 (DTR 180.0) 75)) ;hole location on struct
;(command "line" newm20 newm21 newm22 newm23 "")
;(command "_insert" "16c" newm24 "" "" "")
(setq pp2 newm24)
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new struct member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))
))
;********************************************************
(setq newst_mem1y1 new_mem_y1) (setq newst_mem1y2 new_mem_y2) (setq newst_mem1type1 newm_memtype) (setq newst_mem1size1 newm_memsize) 
(setq cc1_d1_x1 cc1_d1) (setq cc1_d1_x2 cc1_d2) (setq cc1_d1_r R) (setq newstplt_len_x1 (Number_Round(+ tempcal 75) 1)) (setq newstplt_len_x2 (Number_Round tempcal 1))
;********************************************************
));opt1,opt2
(if (and (= newst_mem_onpole 2) (= newst_mem_onstruct 2))
(progn
(setq index 0)
(repeat 2
(setq new_mem_y1 (getint "\nEnter the vertical distance from slab to new member on pole:"))
(setq new_mem_y2 (getint "\nEnter the vertical distance from slab to new member on struct:"))

(setq newm1 (polar polestarting (DTR 90.0) new_mem_y1)) ;point for new member on pole
(setq newm2 (polar polestarting (DTR 90.0) new_mem_y2)) ;referance point for new member on struct
(setq newm3 (polar newm2 (DTR 0.0) 10000))
(setq newm4 (inters structending structstarting newm2 newm3)) ;point for new member on struct

(if (= polejoints 0) (setq diameter diameter_1))
(if (= polejoints 1) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if (> new_mem_y1 joint_1) (setq diameter diameter_2))))
(if (= polejoints 2) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if ( and (> new_mem_y1 joint_1) (< new_mem_y1 joint_2)) (setq diameter diameter_2)) (if (> new_mem_y1 joint_2) (setq diameter diameter_3))))

(setq ccin newm1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location

(setq newm_ang1 (angle structstarting structending))
(setq newm_ang2 (rtd newm_ang1))
(setq newm_ang3 (- 180 newm_ang2))
(setq newm_ang4 (DTR newm_ang3))

(if (= newstud_membertype "A") 
(progn 
;(command "_insert" "16c" newm4 "" "" "")
(setq pp2 newm4) 
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new struct member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))))

(if (= newstud_membertype "P") 
(progn (setq newm11 (/ structsize 2)) (setq newm12 (/ newm11 (sin newm_ang4)))
(setq tempcal (sqrt (- (* (/ 80 (sin newm_ang4)) (/ 80 (sin newm_ang4))) (* 80 80) )))
(setq newm13 (polar newm4 (DTR 0.0) newm12))
(setq newm14 (polar newm13 newm_ang1 (/ 40 (sin newm_ang4))))
(setq newm15 (polar newm14 (DTR 180.0) 75))
(setq newm16 (polar newm15 (DTR 270.0) 80))
(setq newm17 (polar newm16 (DTR 0.0) (+ tempcal 75)))
(setq newm18ttt (polar newm14 (DTR 180.0) 50))
(setq newm18 (polar newm18ttt (DTR 270.0) 40)) ;hole location on struct
;(command "line" newm14 newm15 newm16 newm17 "")
;(command "_insert" "16c" newm18 "" "" "")
(setq pp2 newm18)
(initget 1 "A P")
(setq newm_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq newm_memsize (getstring "\nEnter the new struct member size::"))
(setq memshoft newm_memsize)
(if (= newm_memtype "A") (angmember))
(if (= newm_memtype "P") (pipemember))))
;********************************************************
(setq index (1+ index))
(if (= index 1) (progn 
(setq newst1_newmem1_y1 new_mem_y1) (setq newst1_newmem1_y2 new_mem_y2) 
(setq newst1_newmem1_type newm_memtype)  
(setq newst1_newmem1_len (Number_Round (distance pp1 pp2) 1))
(setq newst1_newmem1_memname memname)
(setq newst1_newmem1_blockwt blockwt)
(setq newst1_newmem1_backmark bm)
(if (= newm_memtype "P") (progn (setq newst1_newmem1_sizename1 DD3) (setq newst1_newmem1_sizename2 L2) (setq newst1_newmem1_size DD1) (setq newst1_newmem1_thick DDTICK)
(setq newst1_newmem1_stx1 (Number_Round (+ tempcal 100) 1)) ;struct plate for connect new member
(setq newst1_newmem1_stx2 (Number_Round tempcal 1)) ;struct plate for connect new member
))
(if (= newm_memtype "A") (progn  (setq newst1_newmem1_size flange) (setq newst1_newmem1_thick th)))
(setq newst1_newmem1_pclampx1 cc1_d1) 
(setq newst1_newmem1_pclampx2 cc1_d2) 
(setq newst1_newmem1_pclampR R_cc1 )
(setq newst1_joint1_temp1 (Number_Round (distance structstarting newm4) 1))
(setq newst1_joint2_temp1 (Number_Round (distance newm4 structending) 1))
;(setq newmempoint1_onstruct newm4)
))

(if (= index 2) (progn 
(setq newst1_newmem2_y1 new_mem_y1) (setq newst1_newmem2_y2 new_mem_y2) 
(setq newst1_newmem2_type newm_memtype)  
(setq newst1_newmem2_len (distance pp1 pp2))
(setq newst1_newmem2_memname memname)
(setq newst1_newmem2_blockwt blockwt)
(setq newst1_newmem2_backmark bm)
(if (= newm_memtype "P") (progn (setq newst1_newmem2_sizename1 DD3) (setq newst1_newmem2_sizename2 L2) (setq newst1_newmem2_size DD1) (setq newst1_newmem2_thick DDTICK)
(setq newst1_newmem2_stx1 (Number_Round (+ tempcal 100) 1)) ;struct plate for connect new member
(setq newst1_newmem2_stx2 (Number_Round tempcal 1)) ;struct plate for connect new member
))
(if (= newm_memtype "A") (progn  (setq newst1_newmem2_size flange) (setq newst1_newmem2_thick th)))
(setq newst1_newmem2_pclampx1 cc1_d1) 
(setq newst1_newmem2_pclampx2 cc1_d2) 
(setq newst1_newmem2_pclampR R_cc1 )

(setq newst1_joint1_temp2 (Number_Round (distance structstarting newm4) 1))
(setq newst1_joint2_temp2 (Number_Round (distance newm4 structending) 1))

(if (> (distance structstarting structending) 6000) (progn
		(if (> newst1_joint1_temp2 newst1_joint1_temp1) (progn (setq newst1_joint1 newst1_joint1_temp1) (setq newst1_joint2 (- newst1_joint1_temp2 newst1_joint2_temp2)) (setq newst1_joint3 newst1_joint2_temp2)))
		(if (> newst1_joint1_temp1 newst1_joint1_temp2) (progn (setq newst1_joint1 newst1_joint1_temp2) (setq newst1_joint2 (- newst1_joint2_temp2 newst1_joint2_temp1)) (setq newst1_joint3 newst1_joint2_temp1)))
	))
	(if (< (distance structstarting structending) 6000) (progn
		(if (> newst1_joint1_temp2 newst1_joint1_temp1) (progn (setq newst1_joint1 newst1_joint1_temp1) (setq newst1_joint2_1 (/ (- newst1_joint1_temp2 newst1_joint2_temp2) 2)) (setq newst1_joint2_2 (/ (- newst1_joint1_temp2 newst1_joint2_temp2) 2)) (setq newst1_joint3 newst1_joint2_temp2)))
		(if (> newst1_joint1_temp1 newst1_joint1_temp2) (progn (setq newst1_joint1 newst1_joint1_temp2) (setq newst1_joint2_1 (/ (- newst1_joint2_temp2 newst1_joint2_temp1) 2)) (setq newst1_joint2_2 (/ (- newst1_joint2_temp2 newst1_joint2_temp1) 2)) (setq newst1_joint3 newst1_joint2_temp1)))
	))
;(setq newmempoint2_onstruct newm4)
;(if (> newst1_newmem1_y1 newst1_newmem2_y1) (progn (setq newst1_joint1 (Number_Round (distance structstarting newmempoint1_onstruct) 1)) (setq newst1_joint2 (Number_Round (- (distance structstarting newmempoint2_onstruct) (distance structstarting newmempoint1_onstruct)) 1)) (setq newst1_joint3 (Number_Round (distance structending newmempoint2_onstruct) 1))))
;(if (> newst1_newmem2_y1 newst1_newmem1_y1) (progn (setq newst1_joint1 (Number_Round (distance structstarting newmempoint2_onstruct) 1)) (setq newst1_joint2 (Number_Round (- (distance structstarting newmempoint1_onstruct) (distance structstarting newmempoint2_onstruct)) 1)) (setq newst1_joint3 (Number_Round (distance structending newmempoint1_onstruct) 1))))
))
;********************************************************
)));opt2,opt2
))))
(defun angmember() 
  (setq ang (angle pp1 pp2))
  (setq shearedge 25)
(setq ang1 (+ ang (dtr 90)))
	;(setq memshoft (itoa memshoft))
(if (= memshoft "405") (progn (setq flange 40) (setq bm 22) (setq th 5) (setq memname "L40x40x5") (setq blockwt 3.0) ))
(if (= memshoft "453") (progn (setq flange 45) (setq bm 23) (setq th 3) (setq memname "L45x45x3") (setq blockwt 3.5) ))
(if (= memshoft "454") (progn (setq flange 45) (setq bm 23) (setq th 4) (setq memname "L45x45x4") (setq blockwt 2.7) ))
(if (= memshoft "455") (progn (setq flange 45) (setq bm 23) (setq th 5) (setq memname "L45x45x5") (setq blockwt 3.4) ))
(if (= memshoft "503") (progn (setq flange 50) (setq bm 28) (setq th 3) (setq memname "L50x50x3") (setq blockwt 2.3) ))
(if (= memshoft "504") (progn (setq flange 50) (setq bm 28) (setq th 4) (setq memname "L50x50x4") (setq blockwt 3.0) ))
(if (= memshoft "505") (progn (setq flange 50) (setq bm 28) (setq th 5) (setq memname "L50x50x5") (setq blockwt 3.8) ))
(if (= memshoft "554") (progn (setq flange 55) (setq bm 33) (setq th 4) (setq memname "L55x55x4") (setq blockwt 3.3) ))
(if (= memshoft "555") (progn (setq flange 55) (setq bm 33) (setq th 5) (setq memname "L55x55x5") (setq blockwt 4.1) ))
(if (= memshoft "556") (progn (setq flange 55) (setq bm 33) (setq th 6) (setq memname "L55x55x6") (setq blockwt 4.9) ))
(if (= memshoft "604") (progn (setq flange 60) (setq bm 35) (setq th 4) (setq memname "L60x60x4") (setq blockwt 3.7) ))
(if (= memshoft "605") (progn (setq flange 60) (setq bm 35) (setq th 5) (setq memname "L60x60x5") (setq blockwt 4.5) ))
(if (= memshoft "606") (progn (setq flange 60) (setq bm 35) (setq th 6) (setq memname "L60x60x6") (setq blockwt 5.4) ))
(if (= memshoft "654") (progn (setq flange 65) (setq bm 38) (setq th 4) (setq memname "L65x65x4") (setq blockwt 4.0) ))
(if (= memshoft "655") (progn (setq flange 65) (setq bm 38) (setq th 5) (setq memname "L65x65x5") (setq blockwt 4.9) ))
(if (= memshoft "656") (progn (setq flange 65) (setq bm 38) (setq th 6) (setq memname "L65x65x6") (setq blockwt 5.8) ))
(if (= memshoft "705") (progn (setq flange 70) (setq bm 40) (setq th 5) (setq memname "L70x70x5") (setq blockwt 5.3) ))
(if (= memshoft "706") (progn (setq flange 70) (setq bm 40) (setq th 6) (setq memname "L70x70x6") (setq blockwt 6.3) ))
(if (= memshoft "755") (progn (setq flange 75) (setq bm 40) (setq th 5) (setq memname "L75x75x5") (setq blockwt 5.7) ))
(if (= memshoft "756") (progn (setq flange 75) (setq bm 40) (setq th 6) (setq memname "L75x75x6") (setq blockwt 6.8) ))
(if (= memshoft "806") (progn (setq flange 80) (setq bm 45) (setq th 6) (setq memname "L80x80x6") (setq blockwt 7.3) ))
(if (= memshoft "808") (progn (setq flange 80) (setq bm 45) (setq th 8) (setq memname "L80x80x8") (setq blockwt 9.6) ))
(if (= memshoft "906") (progn (setq flange 90) (setq bm 50) (setq th 6) (setq memname "L90x90x6") (setq blockwt 8.2) ))
(if (= memshoft "908") (progn (setq flange 90) (setq bm 50) (setq th 8) (setq memname "L90x90x8") (setq blockwt 10.8) ))
(if (= memshoft "1006") (progn (setq flange 100) (setq bm 60) (setq th 6) (setq memname "100x100x6") (setq blockwt 9.2) ))
(if (= memshoft "1008") (progn (setq flange 100) (setq bm 60) (setq th 8) (setq memname "100x100x8") (setq blockwt 12.1) ))
(if (= memshoft "10010") (progn (setq flange 100) (setq bm 60) (setq th 10) (setq memname "100x100x10") (setq blockwt 14.9) ))
(if (= memshoft "1108") (progn (setq flange 110) (setq bm 65) (setq th 8) (setq memname "110x110x8") (setq blockwt 13.4) ))
(if (= memshoft "11010") (progn (setq flange 110) (setq bm 65) (setq th 10) (setq memname "110x110x10") (setq blockwt 16.6) ))
(if (= memshoft "1308") (progn (setq flange 130) (setq bm 80) (setq th 8) (setq memname "130x130x10") (setq blockwt 15.9) ))
(if (= memshoft "13010") (progn (setq flange 130) (setq bm 80) (setq th 10) (setq memname "130x130x10") (setq blockwt 19.7) ))
(if (= memshoft "13012") (progn (setq flange 130) (setq bm 80) (setq th 12) (setq memname "130x130x12") (setq blockwt 23.5) ))
(if (= memshoft "15010") (progn (setq flange 150) (setq bm 90) (setq th 10) (setq memname "150x150x12") (setq blockwt 22.9) ))
(if (= memshoft "15012") (progn (setq flange 150) (setq bm 90) (setq th 12) (setq memname "150x150x12") (setq blockwt 27.3) ))
	
    (setq pp3ref (polar pp1 ang1 (- bm th))
             pp3 (polar pp3ref (angle pp2 pp1) shearedge)
          pp4ref (polar pp2 ang1 (- bm th))
             pp4 (polar pp4ref (angle pp1 pp2) shearedge)
          pp5ref (polar pp1 ang1 bm)
             pp5 (polar pp5ref (angle pp2 pp1) shearedge)
          pp6ref (polar pp2 ang1 bm)
             pp6 (polar pp6ref (angle pp1 pp2) shearedge)
          pp7ref (polar pp1 (+ ang1 (dtr 180)) (- flange bm))
             pp7 (polar pp7ref (angle pp2 pp1) shearedge)
          pp8ref (polar pp2 (+ ang1 (dtr 180)) (- flange bm))
             pp8 (polar pp8ref (angle pp1 pp2) shearedge)
    ) 
    ;(command "layer" "s" "mem" "")
    ;(command "line" pp3 pp4 "")
    ;(command "chprop" "l" "" "la" "das" "")
    ;(command "line" pp5 pp6 "")
    ;(command "line" pp7 pp8 "")
    ;(command "line" pp7 pp5 "")
    ;(command "line" pp6 pp8 "")

)

(defun pipemember() 

(setq ang (angle pp1 pp2)) ;for 0
(setq shearedge 25)
(setq ang1 (+ ang (dtr 90)))   ; for 90
(setq ang2 (angle pp2 pp1))    ; for 0 to 180
(setq ang3 (+ ang1 (dtr 180))) ;for 270
	
(if (= memshoft "48.3x2.9") (progn (setq DD1 48.3) (setq DD2 68.4) (setq bm 34.2) (setq DD3 (strcat "48.3" "%%c")) (setq L1 60) (setq L2 72) (setq TT1 22) (setq DDTICK 2.9) (setq memname "PIPE48.3x2.9Thk") (setq blockwt 3.23)))
(if (= memshoft "48.3x3.2") (progn (setq DD1 48.3) (setq DD2 68.4) (setq bm 34.2) (setq DD3 (strcat "48.3" "%%c")) (setq L1 60) (setq L2 72) (setq TT1 22) (setq DDTICK 3.2) (setq memname "PIPE48.3x3.2Thk") (setq blockwt 3.56)))
(if (= memshoft "48.3x4.0") (progn (setq DD1 48.3) (setq DD2 68.4) (setq bm 34.2) (setq DD3 (strcat "48.3" "%%c")) (setq L1 60) (setq L2 72) (setq TT1 22) (setq DDTICK 4.0) (setq memname "PIPE48.3x4.0Thk") (setq blockwt 4.37)))
(if (= memshoft "60.3x2.9") (progn (setq DD1 60.3) (setq DD2 80) (setq bm 40) (setq DD3 (strcat "60.3" "%%c")) (setq L1 60) (setq L2 90) (setq TT1 15) (setq DDTICK 2.9) (setq memname "PIPE60.3x2.9Thk") (setq blockwt 4.08)))
(if (= memshoft "60.3x3.6") (progn (setq DD1 60.3) (setq DD2 80) (setq bm 40) (setq DD3 (strcat "60.3" "%%c")) (setq L1 60) (setq L2 90) (setq TT1 15) (setq DDTICK 3.6) (setq memname "PIPE60.3x3.6Thk") (setq blockwt 5.03)))
(if (= memshoft "60.3x4.5") (progn (setq DD1 60.3) (setq DD2 80) (setq bm 40) (setq DD3 (strcat "60.3" "%%c")) (setq L1 60) (setq L2 90) (setq TT1 15) (setq DDTICK 4.5) (setq memname "PIPE60.3x4.5Thk") (setq blockwt 6.19)))
(if (= memshoft "76.1x3.2") (progn (setq DD1 76.1) (setq DD2 107) (setq bm 53.5) (setq DD3 (strcat "76.1" "%%c")) (setq L1 60) (setq L2 114) (setq TT1 20) (setq DDTICK 3.2) (setq memname "PIPE76.1x3.2Thk")(setq blockwt 5.71)))
(if (= memshoft "76.1x3.6") (progn (setq DD1 76.1) (setq DD2 107) (setq bm 53.5) (setq DD3 (strcat "76.1" "%%c")) (setq L1 60) (setq L2 114) (setq TT1 20) (setq DDTICK 3.6) (setq memname "PIPE76.1x3.6Thk")(setq blockwt 6.42)))
(if (= memshoft "76.1x4.5") (progn (setq DD1 76.1) (setq DD2 107) (setq bm 53.5) (setq DD3 (strcat "76.1" "%%c")) (setq L1 60) (setq L2 114) (setq TT1 20) (setq DDTICK 4.5) (setq memname "PIPE76.1x4.5Thk")(setq blockwt 7.93)))
(if (= memshoft "88.9x3.2") (progn (setq DD1 88.9) (setq DD2 127) (setq bm 63.5) (setq DD3 (strcat "88.9" "%%c")) (setq L1 60) (setq L2 133) (setq TT1 39) (setq DDTICK 3.2) (setq memname "PIPE88.9x3.2Thk") (setq blockwt 6.72)))
(if (= memshoft "88.9x4.0") (progn (setq DD1 88.9) (setq DD2 127) (setq bm 63.5) (setq DD3 (strcat "88.9" "%%c")) (setq L1 60) (setq L2 133) (setq TT1 39) (setq DDTICK 4.0) (setq memname "PIPE88.9x4.0Thk") (setq blockwt 8.36)))
(if (= memshoft "88.9x4.8") (progn (setq DD1 88.9) (setq DD2 127) (setq bm 63.5) (setq DD3 (strcat "88.9" "%%c")) (setq L1 60) (setq L2 133) (setq TT1 39) (setq DDTICK 4.8) (setq memname "PIPE88.9x4.8Thk") (setq blockwt 9.90)))
(if (= memshoft "101.6x3.6") (progn (setq DD1 101.6) (setq DD2 147) (setq bm 73.5) (setq DD3 (strcat "101.6" "%%c")) (setq L1 60) (setq L2 152) (setq TT1 45) (setq DDTICK 3.6) (setq memname "PIPE101.6x3.6Thk") (setq blockwt 8.70)))
(if (= memshoft "101.6x4.0") (progn (setq DD1 101.6) (setq DD2 147) (setq bm 73.5) (setq DD3 (strcat "101.6" "%%c")) (setq L1 60) (setq L2 152) (setq TT1 45) (setq DDTICK 4.0) (setq memname "PIPE101.6x4.0Thk") (setq blockwt 9.63)))
(if (= memshoft "101.6x4.8") (progn (setq DD1 101.6) (setq DD2 147) (setq bm 73.5) (setq DD3 (strcat "101.6" "%%c")) (setq L1 60) (setq L2 152) (setq TT1 45) (setq DDTICK 4.8) (setq memname "PIPE101.6x4.8Thk") (setq blockwt 11.5)))
(if (= memshoft "114.3x3.6") (progn (setq DD1 114.3) (setq DD2 167) (setq bm 83.5) (setq DD3 (strcat "114.3" "%%c")) (setq L1 60) (setq L2 172) (setq TT1 50) (setq DDTICK 3.6) (setq memname "PIPE114.3x3.6Thk") (setq blockwt 9.75)))
(if (= memshoft "114.3x4.5") (progn (setq DD1 114.3) (setq DD2 167) (setq bm 83.5) (setq DD3 (strcat "114.3" "%%c")) (setq L1 60) (setq L2 172) (setq TT1 50) (setq DDTICK 4.5) (setq memname "PIPE114.3x4.5Thk") (setq blockwt 12.2)))
(if (= memshoft "114.3x5.4") (progn (setq DD1 114.3) (setq DD2 167) (setq bm 83.5) (setq DD3 (strcat "114.3" "%%c")) (setq L1 60) (setq L2 172) (setq TT1 50) (setq DDTICK 5.4) (setq memname "PIPE114.3x5.4Thk") (setq blockwt 14.5)))
(if (= memshoft "139.7x4.5") (progn (setq DD1 139.7) (setq DD2 207) (setq bm 103.5) (setq DD3 (strcat "139.7" "%%c")) (setq L1 60) (setq L2 210) (setq TT1 60) (setq DDTICK 4.5) (setq memname "PIPE139.7x4.5Thk") (setq blockwt 15.0)))
(if (= memshoft "139.7x4.8") (progn (setq DD1 139.7) (setq DD2 207) (setq bm 103.5) (setq DD3 (strcat "139.7" "%%c")) (setq L1 60) (setq L2 210) (setq TT1 60) (setq DDTICK 4.8) (setq memname "PIPE139.7x4.8Thk") (setq blockwt 15.9)))
(if (= memshoft "139.7x5.4") (progn (setq DD1 139.7) (setq DD2 207) (setq bm 103.5) (setq DD3 (strcat "139.7" "%%c")) (setq L1 60) (setq L2 210) (setq TT1 60) (setq DDTICK 5.4) (setq memname "PIPE139.7x5.4Thk") (setq blockwt 17.9)))
	
    (setq pp3 (polar pp1 ang2 shearedge))
	(setq pp4 (polar pp3 ang1 TT1))
	(setq pp5 (polar pp3 ang1 (/ DD2 2)))
	(setq pp6 (polar pp3 ang3 TT1))
	(setq pp7 (polar pp3 ang3 (/ DD2 2)))
	(setq pp8 (polar pp7 ang L1))
	(setq pp9 (polar pp5 ang L1))
	(setq pp10 (polar pp3 ang (+ L1 L2)))
	(setq pp11 (polar pp10 ang1 (/ DD1 2)))
	(setq pp12 (polar pp10 ang3 (/ DD1 2)))
	(setq pp13 (polar pp2 ang shearedge))
	(setq pp14 (polar pp13 ang1 TT1))
	(setq pp15 (polar pp13 ang1 (/ DD2 2)))
	(setq pp16 (polar pp13 ang3 TT1))
	(setq pp17 (polar pp13 ang3 (/ DD2 2)))
	(setq pp18 (polar pp17 ang2 L1))
	(setq pp19 (polar pp15 ang2 L1))
	(setq pp20 (polar pp13 ang2 (+ L1 L2)))
	(setq pp21 (polar pp20 ang1 (/ DD1 2)))
	(setq pp22 (polar pp20 ang3 (/ DD1 2)))
    (setq pp23 (polar pp1 ang1 TT1))	
    (setq pp24 (polar pp1 ang3 TT1))	
    (setq pp25 (polar pp1 ang TT1))
    (setq pp26 (polar pp2 ang1 TT1))	
    (setq pp27 (polar pp2 ang2 TT1))	
    (setq pp28 (polar pp2 ang3 TT1))
	
    (setq pp29 (polar pp5 ang3 DDTICK))	;for outer line offset
    (setq pp30 (polar pp9 ang3 DDTICK))	 ;for outer line offset
    (setq pp31 (polar pp11 ang3 DDTICK)) ;for outer line offset	
    (setq pp32 (polar pp21 ang3 DDTICK)) ;for outer line offset	
    (setq pp33 (polar pp19 ang3 DDTICK)) ;for outer line offset	
    (setq pp34 (polar pp15 ang3 DDTICK)) ;for outer line offset	
	
    (setq pp35 (polar pp7 ang1 DDTICK))	;for outer line offset
    (setq pp36 (polar pp8 ang1 DDTICK))	;for outer line offset
    (setq pp37 (polar pp12 ang1 DDTICK)) ;for outer line offset	
    (setq pp38 (polar pp22 ang1 DDTICK)) ;for outer line offset
    (setq pp39 (polar pp18 ang1 DDTICK)) ;for outer line offset
    (setq pp40 (polar pp17 ang1 DDTICK)) ;for outer line offset

    ;(command "layer" "s" "mem" "")
	;(command "line" pp7 pp5 pp9 pp11 pp21 pp19 pp15 pp17 pp18 pp22 pp12 pp8 pp7 "")
    ;(command "layer" "s" "das" "")
	;(command "line" pp29 pp30 pp31 pp32 pp33 pp34 "")
	;(command "line" pp35 pp36 pp37 pp38 pp39 pp40 "")
    ;(command "layer" "s" "gus" "")
	;(command "line" pp4 pp23 "")
	;(command "line" pp6 pp24 "")
	;(command "line" pp14 pp26 "")
	;(command "line" pp16 pp28 "")
	;(command "arc" pp23 pp25 pp24 "")
	;(command "arc" pp26 pp27 pp28 "")
	;(command "layer" "s" "mem" "")
	
	)
	
(defun pipemember_twobolts() 

(setq ang (angle pp1 pp2)) ;for 0
(setq shearedge 25)
(setq ang1 (+ ang (dtr 90)))   ; for 90
(setq ang2 (angle pp2 pp1))    ; for 0 to 180
(setq ang3 (+ ang1 (dtr 180))) ;for 270
	
(if (= memshoft "48.3x2.9") (progn (setq DD1 48.3) (setq DD2 68.4) (setq bm 34.2) (setq DD3 (strcat "48.3" "%%c")) (setq L1 100) (setq L2 72) (setq TT1 22) (setq DDTICK 2.9) (setq memname "PIPE48.3x2.9Thk") (setq blockwt 3.23)))
(if (= memshoft "48.3x3.2") (progn (setq DD1 48.3) (setq DD2 68.4) (setq bm 34.2) (setq DD3 (strcat "48.3" "%%c")) (setq L1 100) (setq L2 72) (setq TT1 22) (setq DDTICK 3.2) (setq memname "PIPE48.3x3.2Thk") (setq blockwt 3.56)))
(if (= memshoft "48.3x4.0") (progn (setq DD1 48.3) (setq DD2 68.4) (setq bm 34.2) (setq DD3 (strcat "48.3" "%%c")) (setq L1 100) (setq L2 72) (setq TT1 22) (setq DDTICK 4.0) (setq memname "PIPE48.3x4.0Thk") (setq blockwt 4.37)))
(if (= memshoft "60.3x2.9") (progn (setq DD1 60.3) (setq DD2 80) (setq bm 40) (setq DD3 (strcat "60.3" "%%c")) (setq L1 100) (setq L2 90) (setq TT1 15) (setq DDTICK 2.9) (setq memname "PIPE60.3x2.9Thk") (setq blockwt 4.08)))
(if (= memshoft "60.3x3.6") (progn (setq DD1 60.3) (setq DD2 80) (setq bm 40) (setq DD3 (strcat "60.3" "%%c")) (setq L1 100) (setq L2 90) (setq TT1 15) (setq DDTICK 3.6) (setq memname "PIPE60.3x3.6Thk") (setq blockwt 5.03)))
(if (= memshoft "60.3x4.5") (progn (setq DD1 60.3) (setq DD2 80) (setq bm 40) (setq DD3 (strcat "60.3" "%%c")) (setq L1 100) (setq L2 90) (setq TT1 15) (setq DDTICK 4.5) (setq memname "PIPE60.3x4.5Thk") (setq blockwt 6.19)))
(if (= memshoft "76.1x3.2") (progn (setq DD1 76.1) (setq DD2 107) (setq bm 53.5) (setq DD3 (strcat "76.1" "%%c")) (setq L1 100) (setq L2 114) (setq TT1 20) (setq DDTICK 3.2) (setq memname "PIPE76.1x3.2Thk")(setq blockwt 5.71)))
(if (= memshoft "76.1x3.6") (progn (setq DD1 76.1) (setq DD2 107) (setq bm 53.5) (setq DD3 (strcat "76.1" "%%c")) (setq L1 100) (setq L2 114) (setq TT1 20) (setq DDTICK 3.6) (setq memname "PIPE76.1x3.6Thk")(setq blockwt 6.42)))
(if (= memshoft "76.1x4.5") (progn (setq DD1 76.1) (setq DD2 107) (setq bm 53.5) (setq DD3 (strcat "76.1" "%%c")) (setq L1 100) (setq L2 114) (setq TT1 20) (setq DDTICK 4.5) (setq memname "PIPE76.1x4.5Thk")(setq blockwt 7.93)))
(if (= memshoft "88.9x3.2") (progn (setq DD1 88.9) (setq DD2 127) (setq bm 63.5) (setq DD3 (strcat "88.9" "%%c")) (setq L1 100) (setq L2 133) (setq TT1 39) (setq DDTICK 3.2) (setq memname "PIPE88.9x3.2Thk") (setq blockwt 6.72)))
(if (= memshoft "88.9x4.0") (progn (setq DD1 88.9) (setq DD2 127) (setq bm 63.5) (setq DD3 (strcat "88.9" "%%c")) (setq L1 100) (setq L2 133) (setq TT1 39) (setq DDTICK 4.0) (setq memname "PIPE88.9x4.0Thk") (setq blockwt 8.36)))
(if (= memshoft "88.9x4.8") (progn (setq DD1 88.9) (setq DD2 127) (setq bm 63.5) (setq DD3 (strcat "88.9" "%%c")) (setq L1 100) (setq L2 133) (setq TT1 39) (setq DDTICK 4.8) (setq memname "PIPE88.9x4.8Thk") (setq blockwt 9.90)))
(if (= memshoft "101.6x3.6") (progn (setq DD1 101.6) (setq DD2 147) (setq bm 73.5) (setq DD3 (strcat "101.6" "%%c")) (setq L1 100) (setq L2 152) (setq TT1 45) (setq DDTICK 3.6) (setq memname "PIPE101.6x3.6Thk") (setq blockwt 8.70)))
(if (= memshoft "101.6x4.0") (progn (setq DD1 101.6) (setq DD2 147) (setq bm 73.5) (setq DD3 (strcat "101.6" "%%c")) (setq L1 100) (setq L2 152) (setq TT1 45) (setq DDTICK 4.0) (setq memname "PIPE101.6x4.0Thk") (setq blockwt 9.63)))
(if (= memshoft "101.6x4.8") (progn (setq DD1 101.6) (setq DD2 147) (setq bm 73.5) (setq DD3 (strcat "101.6" "%%c")) (setq L1 100) (setq L2 152) (setq TT1 45) (setq DDTICK 4.8) (setq memname "PIPE101.6x4.8Thk") (setq blockwt 11.5)))
(if (= memshoft "114.3x3.6") (progn (setq DD1 114.3) (setq DD2 167) (setq bm 83.5) (setq DD3 (strcat "114.3" "%%c")) (setq L1 100) (setq L2 172) (setq TT1 50) (setq DDTICK 3.6) (setq memname "PIPE114.3x3.6Thk") (setq blockwt 9.75)))
(if (= memshoft "114.3x4.5") (progn (setq DD1 114.3) (setq DD2 167) (setq bm 83.5) (setq DD3 (strcat "114.3" "%%c")) (setq L1 100) (setq L2 172) (setq TT1 50) (setq DDTICK 4.5) (setq memname "PIPE114.3x4.5Thk") (setq blockwt 12.2)))
(if (= memshoft "114.3x5.4") (progn (setq DD1 114.3) (setq DD2 167) (setq bm 83.5) (setq DD3 (strcat "114.3" "%%c")) (setq L1 100) (setq L2 172) (setq TT1 50) (setq DDTICK 5.4) (setq memname "PIPE114.3x5.4Thk") (setq blockwt 14.5)))
(if (= memshoft "139.7x4.5") (progn (setq DD1 139.7) (setq DD2 207) (setq bm 103.5) (setq DD3 (strcat "139.7" "%%c")) (setq L1 100) (setq L2 210) (setq TT1 60) (setq DDTICK 4.5) (setq memname "PIPE139.7x4.5Thk") (setq blockwt 15.0)))
(if (= memshoft "139.7x4.8") (progn (setq DD1 139.7) (setq DD2 207) (setq bm 103.5) (setq DD3 (strcat "139.7" "%%c")) (setq L1 100) (setq L2 210) (setq TT1 60) (setq DDTICK 4.8) (setq memname "PIPE139.7x4.8Thk") (setq blockwt 15.9)))
(if (= memshoft "139.7x5.4") (progn (setq DD1 139.7) (setq DD2 207) (setq bm 103.5) (setq DD3 (strcat "139.7" "%%c")) (setq L1 100) (setq L2 210) (setq TT1 60) (setq DDTICK 5.4) (setq memname "PIPE139.7x5.4Thk") (setq blockwt 17.9)))
	
    (setq pp3 (polar pp1 ang2 shearedge))
	(setq pp4 (polar pp3 ang1 TT1))
	(setq pp5 (polar pp3 ang1 (/ DD2 2)))
	(setq pp6 (polar pp3 ang3 TT1))
	(setq pp7 (polar pp3 ang3 (/ DD2 2)))
	(setq pp8 (polar pp7 ang L1))
	(setq pp9 (polar pp5 ang L1))
	(setq pp10 (polar pp3 ang (+ L1 L2)))
	(setq pp11 (polar pp10 ang1 (/ DD1 2)))
	(setq pp12 (polar pp10 ang3 (/ DD1 2)))
	(setq pp13 (polar pp2 ang shearedge))
	(setq pp14 (polar pp13 ang1 TT1))
	(setq pp15 (polar pp13 ang1 (/ DD2 2)))
	(setq pp16 (polar pp13 ang3 TT1))
	(setq pp17 (polar pp13 ang3 (/ DD2 2)))
	(setq pp18 (polar pp17 ang2 L1))
	(setq pp19 (polar pp15 ang2 L1))
	(setq pp20 (polar pp13 ang2 (+ L1 L2)))
	(setq pp21 (polar pp20 ang1 (/ DD1 2)))
	(setq pp22 (polar pp20 ang3 (/ DD1 2)))
    (setq pp23 (polar pp1 ang1 TT1))	
    (setq pp24 (polar pp1 ang3 TT1))	
    (setq pp25 (polar pp1 ang TT1))
    (setq pp26 (polar pp2 ang1 TT1))	
    (setq pp27 (polar pp2 ang2 TT1))	
    (setq pp28 (polar pp2 ang3 TT1))
	
    (setq pp29 (polar pp5 ang3 DDTICK))	
    (setq pp30 (polar pp9 ang3 DDTICK))	
    (setq pp31 (polar pp11 ang3 DDTICK))	
    (setq pp32 (polar pp21 ang3 DDTICK))	
    (setq pp33 (polar pp19 ang3 DDTICK))	
    (setq pp34 (polar pp15 ang3 DDTICK))	
	
    (setq pp35 (polar pp7 ang1 DDTICK))	
    (setq pp36 (polar pp8 ang1 DDTICK))	
    (setq pp37 (polar pp12 ang1 DDTICK))	
    (setq pp38 (polar pp22 ang1 DDTICK))	
    (setq pp39 (polar pp18 ang1 DDTICK))	
    (setq pp40 (polar pp17 ang1 DDTICK))

    ;(command "layer" "s" "mem" "")
	;(command "line" pp7 pp5 pp9 pp11 pp21 pp19 pp15 pp17 pp18 pp22 pp12 pp8 pp7 "")
    ;(command "layer" "s" "das" "")
	;(command "line" pp29 pp30 pp31 pp32 pp33 pp34 "")
	;(command "line" pp35 pp36 pp37 pp38 pp39 pp40 "")
    ;(command "layer" "s" "gus" "")
	;(command "line" pp4 pp23 "")
	;(command "line" pp6 pp24 "")
	;(command "line" pp14 pp26 "")
	;(command "line" pp16 pp28 "")
	;(command "arc" pp23 pp25 pp24 "")
	;(command "arc" pp26 pp27 pp28 "")
	;(command "layer" "s" "mem" "")
	
	)
(defun amem()
(if (= memshoft "405") (progn (setq flange 40) (setq bm 22) (setq th 5)))
(if (= memshoft "453") (progn (setq flange 45) (setq bm 23) (setq th 3)))
(if (= memshoft "454") (progn (setq flange 45) (setq bm 23) (setq th 4)))
(if (= memshoft "455") (progn (setq flange 45) (setq bm 23) (setq th 5)))
(if (= memshoft "503") (progn (setq flange 50) (setq bm 27) (setq th 3)))
(if (= memshoft "504") (progn (setq flange 50) (setq bm 27) (setq th 4)))
(if (= memshoft "505") (progn (setq flange 50) (setq bm 28) (setq th 5)))
(if (= memshoft "554") (progn (setq flange 55) (setq bm 31) (setq th 4)))
(if (= memshoft "555") (progn (setq flange 55) (setq bm 31) (setq th 5)))
(if (= memshoft "556") (progn (setq flange 55) (setq bm 31) (setq th 6)))
(if (= memshoft "604") (progn (setq flange 60) (setq bm 33) (setq th 4)))
(if (= memshoft "605") (progn (setq flange 60) (setq bm 33) (setq th 5)))
(if (= memshoft "606") (progn (setq flange 60) (setq bm 35) (setq th 6)))
(if (= memshoft "654") (progn (setq flange 65) (setq bm 36) (setq th 4)))
(if (= memshoft "655") (progn (setq flange 65) (setq bm 36) (setq th 5)))
(if (= memshoft "656") (progn (setq flange 65) (setq bm 36) (setq th 6)))
(if (= memshoft "705") (progn (setq flange 70) (setq bm 41) (setq th 5)))
(if (= memshoft "706") (progn (setq flange 70) (setq bm 41) (setq th 6)))
(if (= memshoft "755") (progn (setq flange 75) (setq bm 44) (setq th 5)))
(if (= memshoft "756") (progn (setq flange 75) (setq bm 44) (setq th 6)))
(if (= memshoft "806") (progn (setq flange 80) (setq bm 46) (setq th 6)))
(if (= memshoft "808") (progn (setq flange 80) (setq bm 46) (setq th 8)))
(if (= memshoft "906") (progn (setq flange 90) (setq bm 51) (setq th 6)))
(if (= memshoft "908") (progn (setq flange 90) (setq bm 51) (setq th 8)))
(if (= memshoft "1006") (progn (setq flange 100) (setq bm 56) (setq th 6)))
(if (= memshoft "1008") (progn (setq flange 100) (setq bm 56) (setq th 8)))
(if (= memshoft "10010") (progn (setq flange 100) (setq bm 58) (setq th 10)))
(if (= memshoft "1108") (progn (setq flange 110) (setq bm 61) (setq th 8)))
(if (= memshoft "11010") (progn (setq flange 110) (setq bm 61) (setq th 10)))
(if (= memshoft "1308") (progn (setq flange 130) (setq bm 78) (setq th 8)))
(if (= memshoft "13010") (progn (setq flange 130) (setq bm 78) (setq th 10)))
(if (= memshoft "13012") (progn (setq flange 130) (setq bm 78) (setq th 12)))
(if (= memshoft "15010") (progn (setq flange 150) (setq bm 88) (setq th 10)))
(if (= memshoft "15012") (progn (setq flange 150) (setq bm 88) (setq th 12)))
)
(defun p_dia()
(if (= dia "48.3x2.9") (setq dia 48.3))
(if (= dia "48.3x3.2") (setq dia 48.3))
(if (= dia "48.3x4.0") (setq dia 48.3))
(if (= dia "60.3x2.9") (setq dia 60.3))
(if (= dia "60.3x3.6") (setq dia 60.3))
(if (= dia "60.3x4.5") (setq dia 60.3))
(if (= dia "76.1x3.2") (setq dia 76.1))
(if (= dia "76.1x3.6") (setq dia 76.1))
(if (= dia "76.1x4.5") (setq dia 76.1))
(if (= dia "88.9x3.2") (setq dia 88.9))
(if (= dia "88.9x4.0") (setq dia 88.9))
(if (= dia "88.9x4.8") (setq dia 88.9))
(if (= dia "101.6x3.6") (setq dia 101.6))
(if (= dia "101.6x4.0") (setq dia 101.6))
(if (= dia "101.6x4.8") (setq dia 101.6))
(if (= dia "114.3x3.6") (setq dia 114.3))
(if (= dia "114.3x4.5") (setq dia 114.3))
(if (= dia "114.3x5.4") (setq dia 114.3))
(if (= dia "139.7x4.5") (setq dia 139.7))
(if (= dia "139.7x4.8") (setq dia 139.7))
(if (= dia "139.7x5.4") (setq dia 139.7))
)
(defun existing_newm()
(setq existst_new_onpole (getint "\nEnter the number of new members on pole<0/1/2>:"))
(setq existst_new_onstruct (getint "\nEnter the number of new members on existing struct<0/1/2>:"))

(if (and (= existst_new_onpole 1) (= existst_new_onstruct 1))
(progn
(setq new_mem_y1 (getint "\nEnter the vertical distance from slab to new member on pole:"))
(setq new_mem_y2 (getint "\nEnter the vertical distance from slab to new member on struct:"))

(setq nem1 (polar polestarting (DTR 90.0) new_mem_y1)) ;point for new member on pole
(setq nem2 (polar polestarting (DTR 90.0) new_mem_y2)) ;referance point for new member on struct
(setq nem3 (polar nem2 (DTR 0.0) 10000))
(setq nem4 (inters structending structstarting nem2 nem3)) ;point for new member on struct

(if (= polejoints 0) (setq diameter diameter_1))
(if (= polejoints 1) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if (> new_mem_y1 joint_1) (setq diameter diameter_2))))
(if (= polejoints 2) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if ( and (> new_mem_y1 joint_1) (< new_mem_y1 joint_2)) (setq diameter diameter_2)) (if (> new_mem_y1 joint_2) (setq diameter diameter_3))))

(if (= existingstud_membertype "A") 
(progn 
(setq ccin nem1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq aclampin nem4) 
(A_Clamp)
(setq pp2 angle_clamp_hole)
))

(if (= existingstud_membertype "P") 
(progn 
(setq ccin nem1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq ccin nem4) 
(setq pipedia structsize) 
(C-Clamp3)
(setq pp2 angle_clamp_hole)
))
(initget 1 "A P")
(setq nem_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq nem_memsize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft nem_memsize)
(if (= nem_memtype "A") (angmember))
(if (= nem_memtype "P") (pipemember))
;*****************************************
(setq exst_newmem1_type nem_memtype)
(setq exst_newmem1_size nem_memsize)
(setq exst_newmem1_len (distance pp1 pp2))
(setq exst_newmem1_onpoley1 new_mem_y1)
(if (= exst_newmem1_type "P") (progn ))
(if (= exst_newmem1_type "A") (progn ))
(if (= existingstud_membertype "P") (progn (setq exst_poleclamp_x1 cc1_d1) (setq exst_clamp_x2 cc1_d2) (setq exst_stclamp_x1 cc3_d1) (setq exst_stclamp_x2 cc3_d2) ))	
(if (= existingstud_membertype "A") (progn (setq exst_poleclamp_x1 cc1_d1) (setq exst_clamp_x2 cc1_d2) (setq exst_stclamp_x1 cc3_d1) (setq exst_stclamp_x2 cc3_d2) ))
;*****************************************
));opt1,opt1
(if (and (= existst_new_onpole 1) (= existst_new_onstruct 2))
(progn
(setq new_mem_y1 (getint "\nEnter the vertical distance from slab to new member on pole:"))
(setq new_mem_y2 (getint "\nEnter the vertical distance from slab to bollom new member on struct:"))
(setq new_mem_y3 (getint "\nEnter the vertical distance from slab to top new member on struct:"))

(setq nem1 (polar polestarting (DTR 90.0) new_mem_y1)) ;point for new member on pole
(setq nem2 (polar polestarting (DTR 90.0) new_mem_y2))
(setq nem3 (polar nem2 (DTR 0.0) 10000))
(setq nem4 (inters structending structstarting nem2 nem3)) ;point for new member on struct

(setq nem5 (polar polestarting (DTR 90.0) new_mem_y3))
(setq nem6 (polar nem5 (DTR 0.0) 10000))
(setq nem7 (inters structending structstarting nem5 nem6)) ;point for new member on struct

(if (= polejoints 0) (setq diameter diameter_1))
(if (= polejoints 1) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if (> new_mem_y1 joint_1) (setq diameter diameter_2))))
(if (= polejoints 2) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) 
(if ( and (> new_mem_y1 joint_1) (< new_mem_y1 joint_2)) (setq diameter diameter_2)) (if (> new_mem_y1 joint_2) (setq diameter diameter_3)))) 

(if (= existingstud_membertype "A") 
(progn 
(setq ccin nem1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq aclampin nem4) 
(A_Clamp)
(setq pp2 angle_clamp_hole)
(initget 1 "A P")
(setq nem_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq nem_memsize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft nem_memsize)
(if (= nem_memtype "A") (angmember))
(if (= nem_memtype "P") (pipemember))
(setq aclampin nem7) 
(A_Clamp)
(setq pp2 angle_clamp_hole)
(initget 1 "A P")
(setq nem_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq nem_memsize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft nem_memsize)
(if (= nem_memtype "A") (angmember))
(if (= nem_memtype "P") (pipemember))
))
(if (= existingstud_membertype "P") 
(progn 
(setq ccin nem1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq ccin nem4) (setq pipedia structsize) 
(C-Clamp3)
(setq pp2 angle_clamp_hole)
(initget 1 "A P")
(setq nem_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq nem_memsize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft nem_memsize)
(if (= nem_memtype "A") (angmember))
(if (= nem_memtype "P") (pipemember))
(setq ccin nem7) (setq pipedia structsize) 
(C-Clamp3)
(setq pp2 angle_clamp_hole)
(initget 1 "A P")
(setq nem_memtype (getkword "\nEnter the new struct member type <Angle/Pipe>::"))
(setq nem_memsize (getstring "\nEnter the extsting struct member size::"))
(setq memshoft nem_memsize)
(if (= nem_memtype "A") (angmember))
(if (= nem_memtype "P") (pipemember))
))));opt1,opt2	
(if (and (= existst_new_onpole 2) (= existst_new_onstruct 2))
(progn
(setq index 0)
(repeat 2
(setq new_mem_y1 (getint "\nEnter the vertical distance from slab to new member on pole:"))
(setq new_mem_y2 (getint "\nEnter the vertical distance from slab to new member on struct:"))
(setq nem1 (polar polestarting (DTR 90.0) new_mem_y1)) ;point for new member on pole
(setq nem2 (polar polestarting (DTR 90.0) new_mem_y2)) ;referance point for new member on struct
(setq nem3 (polar nem2 (DTR 0.0) 10000))
(setq nem4 (inters structending structstarting nem2 nem3)) ;point for new member on struct
(if (= polejoints 0)(setq diameter diameter_1))
(if (= polejoints 1) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if (> new_mem_y1 joint_1) (setq diameter diameter_2))))
(if (= polejoints 2) (progn (if (< new_mem_y1 joint_1) (setq diameter diameter_1)) (if ( and (> new_mem_y1 joint_1) (< new_mem_y1 joint_2)) (setq diameter diameter_2)) (if (> new_mem_y1 joint_2) (setq diameter diameter_3))))

(if (= existingstud_membertype "A") 
(progn 
(setq ccin nem1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq aclampin nem4) 
(A_Clamp)
(setq pp2 angle_clamp_hole)
))
(if (= existingstud_membertype "P") 
(progn 
(setq ccin nem1) (setq pipedia diameter) 
(C-Clamp1)
(setq pp1 pole_clamp_hole) ;pipe clamp hole location
(setq ccin nem4) (setq pipedia structsize) 
(C-Clamp3)
(setq pp2 angle_clamp_hole)
))

(initget 1 "A P")
(setq nem_memtype (getkword "\nEnter the new member type <Angle/Pipe>::"))
(setq nem_memsize (getstring "\nEnter the new member size::"))
(setq memshoft nem_memsize)
(if (= nem_memtype "A") (angmember))
(if (= nem_memtype "P") (pipemember))
(setq index 0 )
)));opt2,opt2
);;end
(defun C-Clamp1() 
;place ccin and pipedia value pipckers in main program
(setq pipethk 3)

		(setq c1 (polar ccin (DTR 0.0) (+ (/ pipedia 2)7))			  
              c2 (polar c1 (DTR 90.0) 65)
			  c3 (polar c2 (DTR 180.0) (+ pipedia 14))
			  c4 (polar c3 (DTR 270.0) 130)
			  c5 (polar c4 (DTR 0.0) (+ pipedia 14))
			  
			  c6 (polar c1 (DTR 90.0) 50)
			  c7 (polar c6 (DTR 0.0) 100)
			  c8 (polar c7 (DTR 270.0) 100)
			  c9 (polar c8 (DTR 180.0) 100)
			  
			  c10 (polar c8 (DTR 90.0) 50)
			  c11 (polar c10 (DTR 180.0) 30) ;c11 for hole out point
		);setq
;(laygus)
;(command "line" c2 c3 c4 c5 c2  "" "line" c6 c7 c8 c9 c6 "")
;(command "_insert" "16c" c11 "" "" "")
(setq c12 (polar c2 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 )))
(setq c13 (polar c12 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c14 (polar c13 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c15 (polar c14 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))

(setq c16 (polar c5 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 )))
(setq c17 (polar c16 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c18 (polar c17 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c19 (polar c18 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
;(command "line" c12 c16 "" "line" c13 c17 "" "line" c14 c18 "" "line" c15 c19 "")
(setq c20 (polar c2 (DTR 270.0) 25)) 
(setq c21 (polar c20 (DTR 180.0) (+ pipethk 7))) ;line staet
(setq c22 (polar c21 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 ))) ;line end
(setq c23 (polar c5 (DTR 90.0) 25)) 
(setq c24 (polar c23 (DTR 180.0) (+ pipethk 7))) ;line start
(setq c25 (polar c24 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 ))) ;line end
;(command "line" c21 c22 "" "line" c24 c25 "")

(setq pole_clamp_hole c11)
;(if (= pipedia 48.3 ) (progn (setq cc1_d1 ) (setq cc1_d2 )))
(if (= pipedia 48.3 ) (progn (setq cc1_d1 75 ) (setq cc1_d2 138 ) (setq cc1_d3 "60.3 %%c") (setq R_cc1 "R24.2")))
(if (= pipedia 60.3 ) (progn (setq cc1_d1 92 ) (setq cc1_d2 206 ) (setq cc1_d3 "60.3 %%c") (setq R_cc1 "R24.2")))
(if (= pipedia 76.1 ) (progn (setq cc1_d1 116) (setq cc1_d2 230 ) (setq cc1_d3 "76.1 %%c") (setq R_cc1 "R30.2")))
(if (= pipedia 88.9 ) (progn (setq cc1_d1 136) (setq cc1_d2 250 ) (setq cc1_d3 "88.9 %%c") (setq R_cc1 "R44.45")))
(if (= pipedia 114.3 ) (progn (setq cc1_d1 176) (setq cc1_d2 290) (setq cc1_d3 "114.3 %%c") (setq R_cc1 "R57.15")))
(if (= pipedia 139.7 ) (progn (setq cc1_d1 216) (setq cc1_d2 330) (setq cc1_d3 "139.7 %%c") (setq R_cc1 "R70" )))
(if (= pipedia 168.1 ) (progn (setq cc1_d1 261) (setq cc1_d2 375) (setq cc1_d3 "168.1 %%c") (setq R_cc1 "R84.2")))
)
(defun C-Clamp3() 
;place ccin and pipedia value pipckers in main program
(setq ang (angle structstarting structending)) ;for 0
(setq shearedge 25)
(setq ang1 (+ ang (dtr 90.0)))   ; for 90
(setq ang2 (angle structending structstarting))    ; for 0 to 180
(setq ang3 (+ ang1 (dtr 180.0))) ;for 270

(setq ang4 (- (DTR 180.0) ang)) ;for point shifting length

(setq tempcal1 (+ (/ pipedia 2) 57))
(setq tempcal2 (abs (/ tempcal1 (sin ang4))))
(setq tempcal3 (sqrt (- (* tempcal2 tempcal2) (* tempcal1 tempcal1)))) ;shifting length
(setq pipethk 3)

		(setq c1t (polar ccin ang2 tempcal3)
			  c1 (polar c1t ang1 (+ (/ pipedia 2)7))			  
              c2 (polar c1 ang2 65)
			  c3 (polar c2 ang3 (+ pipedia 14))
			  c4 (polar c3 ang 130)
			  c5 (polar c4 ang1 (+ pipedia 14))
			  
			  c6t (polar c3 ang 65)
			  c6 (polar c6t ang2 50)
			  c7 (polar c6 ang3 100)
			  c8 (polar c7 ang 100)
			  c9 (polar c8 ang1 100)
			  
			  c10 (polar c8 ang2 50)
			  c11 (polar c10 ang1 50) ;c11 for hole out point
		);setq
;(laygus)
;(command "line" c2 c3 c4 c5 c2  "" "line" c6 c7 c8 c9 c6 "")
;(command "_insert" "16c" c11 "" "" "")

(setq c12 (polar c3 ang1 (+ (* (/ pipedia 7) 2) pipethk 7 )))
(setq c13 (polar c12 ang3 (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c14 (polar c13 ang3 (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c15 (polar c14 ang3 (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))

(setq c16 (polar c4 ang1 (+ (* (/ pipedia 7) 2) pipethk 7 )))
(setq c17 (polar c16 ang3 (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c18 (polar c17 ang3 (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c19 (polar c18 ang3 (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
;(command "line" c12 c16 "" "line" c13 c17 "" "line" c14 c18 "" "line" c15 c19 "")

(setq c20 (polar c3 ang 25)) 
(setq c21 (polar c20 ang1 (+ pipethk 7))) ;line staet
(setq c22 (polar c21 ang1 (+ (* (/ pipedia 7) 2) pipethk 7 ))) ;line end
(setq c23 (polar c4 ang2 25)) 
(setq c24 (polar c23 ang1 (+ pipethk 7))) ;line start
(setq c25 (polar c24 ang1 (+ (* (/ pipedia 7) 2) pipethk 7 ))) ;line end
;(command "line" c21 c22 "" "line" c24 c25 "")
(setq angle_clamp_hole c11)
;+++++++++++++++++++++++++++
;(if (= pipedia 48.3 ) (progn (setq cc1_d1 ) (setq cc1_d2 )))
(if (= pipedia 48.3 ) (progn (setq cc3_d1 75 ) (setq cc3_d2 138 ) (setq cc3_d3 "48.3 %%c") (setq R_cc3 "R24.2")))
(if (= pipedia 60.3 ) (progn (setq cc3_d1 92 ) (setq cc3_d2 206 ) (setq cc3_d3 "60.3 %%c") (setq R_cc3 "R24.2")))
(if (= pipedia 76.1 ) (progn (setq cc3_d1 116) (setq cc3_d2 230 ) (setq cc3_d3 "76.1 %%c") (setq R_cc3 "R30.2")))
(if (= pipedia 88.9 ) (progn (setq cc3_d1 136) (setq cc3_d2 250 ) (setq cc3_d3 "88.9 %%c") (setq R_cc3 "R44.45")))
(if (= pipedia 114.3 ) (progn (setq cc3_d1 176) (setq cc3_d2 290) (setq cc3_d3 "114.3 %%c") (setq R_cc3 "R57.15")))
(if (= pipedia 139.7 ) (progn (setq cc3_d1 216) (setq cc3_d2 330) (setq cc3_d3 "139.7 %%c") (setq R_cc3 "R70" )))
(if (= pipedia 168.1 ) (progn (setq cc3_d1 261) (setq cc3_d2 375) (setq cc3_d3 "168.1 %%c") (setq R_cc3 "R84.2")))
)
(defun C-Clamp2()
		(setq c1 (polar ccin (DTR 0.0) (+ (/ pipedia 2)7))			  
              c2 (polar c1 (DTR 90.0) 65)
			  c3 (polar c2 (DTR 180.0) (+ pipedia 14))
			  c4 (polar c3 (DTR 270.0) 130)
			  c5 (polar c4 (DTR 0.0) (+ pipedia 14))
			  
			  c6 (polar c1 (DTR 90.0) 50)
			  c7 (polar c6 (DTR 0.0) 150)
			  c8 (polar c7 (DTR 270.0) 100)
			  c9 (polar c8 (DTR 180.0) 150)
			  
			  c10 (polar c8 (DTR 90.0) 25)
			  c11 (polar c10 (DTR 180.0) 25) ;c11 for hole out point
		);setq
(setq pipethk 3)
;(command "line" c2 c3 c4 c5 c2  "" "line" c6 c7 c8 c9 c6 "")
;(command "_insert" "16c" c11 "" "" "")
(setq c12 (polar c2 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 )))
(setq c13 (polar c12 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c14 (polar c13 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c15 (polar c14 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))

(setq c16 (polar c5 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 )))
(setq c17 (polar c16 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c18 (polar c17 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
(setq c19 (polar c18 (DTR 0.0) (/ (+ (* (/ pipedia 7) 2) pipethk 7 ) 4)))
;(command "line" c12 c16 "" "line" c13 c17 "" "line" c14 c18 "" "line" c15 c19 "")
(setq c20 (polar c2 (DTR 270.0) 25)) 
(setq c21 (polar c20 (DTR 180.0) (+ pipethk 7))) ;line staet
(setq c22 (polar c21 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 ))) ;line end
(setq c23 (polar c5 (DTR 90.0) 25)) 
(setq c24 (polar c23 (DTR 180.0) (+ pipethk 7))) ;line start
(setq c25 (polar c24 (DTR 180.0) (+ (* (/ pipedia 7) 2) pipethk 7 ))) ;line end
;(command "line" c21 c22 "" "line" c24 c25 "")
;(if (= pipedia 48.3 ) (progn (setq cc3_d1 ) (setq cc1_d2 )))
(if (= pipedia 48.3 ) (progn (setq cc2_d1 75 ) (setq cc2_d2 138 ) (setq cc2_d3 "48.3 %%c") (setq R_cc3 "R24.2")))
(if (= pipedia 60.3 ) (progn (setq cc2_d1 92 ) (setq cc2_d2 206 ) (setq cc2_d3 "60.3 %%C" ) (setq R_cc2 "R24.2")))
(if (= pipedia 76.1 ) (progn (setq cc2_d1 116) (setq cc2_d2 230 ) (setq cc2_d3 "76.1 %%C") (setq R_cc2 "R30.2")))
(if (= pipedia 88.9 ) (progn (setq cc2_d1 136) (setq cc2_d2 250 ) (setq cc2_d3 "88.9 %%C") (setq R_cc2 "R44.45" )))
(if (= pipedia 114.3 ) (progn (setq cc2_d1 176) (setq cc2_d2 290) (setq cc2_d3 "114.3 %%C") (setq R_cc2 "R57.15")))
(if (= pipedia 139.7 ) (progn (setq cc2_d1 216) (setq cc2_d2 330) (setq cc2_d3 "139.7 %%C") (setq R_cc2 "R70" )))
(if (= pipedia 168.1 ) (progn (setq cc2_d1 261) (setq cc2_d2 375) (setq cc2_d3 "168.1 %%C") (setq R_cc2 "R84.2")))
)
(defun GA-01_twobolts()
(setq GA-01_bplate_wth 230)
(setq GA-01_bplate_ht 230)
(setq GA-01_bplate_tk 10)

(setq GA-01_cleat_wth 150)
(setq GA-01_cleat_ht 150)
(setq GA-01_cleat_tk 10)

(setq ac1 (polar acin (DTR 180.0) (/ GA-01_bplate_wth 2)))
(setq ac2 (polar ac1 (DTR 90.0) GA-01_bplate_tk))
(setq ac3 (polar ac2 (DTR 0.0) GA-01_bplate_wth))
(setq ac4 (polar ac3 (DTR 270.0) GA-01_bplate_tk))

(setq ac5 (polar acin (DTR 90.0) GA-01_bplate_tk)) ;cleat face bottam mid point
(setq ac6 (polar ac5 (DTR 0.0) (/ GA-01_cleat_wth 2)))
(setq ac7 (polar ac6 (DTR 90.0) GA-01_cleat_ht))
(setq ac8 (polar ac7 (DTR 180.0) GA-01_cleat_wth))
(setq ac9 (polar ac8 (DTR 270.0) GA-01_cleat_ht))

(setq ac10 (polar ac8 (DTR 0.0) 25))
(setq ac11 (polar ac10 (DTR 270.0) 25)) ;bolt location
;(command "line" ac1 ac2 ac3 ac4 ac1 "" "line" ac6 ac7 ac8 ac9 ac6 "")
;(command "_insert" "16c" ac11 "" "" "")

(setq ac17 (polar ac7 (DTR 180.0) GA-01_cleat_tk)) ;cleat thick
(setq ac18 (polar ac6 (DTR 180.0) GA-01_cleat_tk)) ;cleat thick
;(command "line" ac17 ac18 "")
;=======================plate dimensions=========;
;(setq ac13 (polar ac11 (DTR 270.0) 100)) ;to find inters
;(setq ac14 (polar ac12 (DTR 180.0) 100)) ;to find inters
;(setq ac15 (inters ac11 ac13 ac14 ac12))
;(setq GA-01_cleat_x (distance ac12 ac15)) ;cleat x diasance for second hole location
;(setq GA-01_cleat_y (distance ac11 ac15)) ;cleat y diasance for second hole location
;(setq GA-01_cleat_x1 25)	
;(setq GA-01_cleat_y1 25)	
;(setq GA-01_cleat_x2 GA-01_cleat_x)	
;(setq GA-01_cleat_y2 GA-01_cleat_y)	
;(setq GA-01_cleat_x3 (- GA-01_cleat_wth (+ GA-01_cleat_x1 GA-01_cleat_x2)))	
;(setq GA-01_cleat_y3 (- GA-01_cleat_ht (+ GA-01_cleat_y1 GA-01_cleat_y2)))
;(setq anchoring_bolt ac12)	
)
(defun A_Clamp()
(setq ang (angle structstarting structending)) ;for 0
(setq shearedge 25)
(setq ang1 (+ ang (dtr 90)))   ; for 90
(setq ang2 (angle structending structstarting))    ; for 0 to 180
(setq ang3 (+ ang1 (dtr 180))) ;for 270
(setq ang4 (- (DTR 180.0) ang)) ;for point shifting length
(setq welding_length 55)
(setq memshoft (fix structsize))   
(if (= memshoft "405") (progn (setq flange 40) (setq bm 22) (setq th 5)))
(if (= memshoft "453") (progn (setq flange 45) (setq bm 23) (setq th 3)))
(if (= memshoft "454") (progn (setq flange 45) (setq bm 23) (setq th 4)))
(if (= memshoft "455") (progn (setq flange 45) (setq bm 23) (setq th 5)))
(if (= memshoft "503") (progn (setq flange 50) (setq bm 27) (setq th 3)))
(if (= memshoft "504") (progn (setq flange 50) (setq bm 27) (setq th 4)))
(if (= memshoft "505") (progn (setq flange 50) (setq bm 28) (setq th 5)))
(if (= memshoft "554") (progn (setq flange 55) (setq bm 31) (setq th 4)))
(if (= memshoft "555") (progn (setq flange 55) (setq bm 31) (setq th 5)))
(if (= memshoft "556") (progn (setq flange 55) (setq bm 31) (setq th 6)))
(if (= memshoft "604") (progn (setq flange 60) (setq bm 33) (setq th 4)))
(if (= memshoft "605") (progn (setq flange 60) (setq bm 33) (setq th 5)))
(if (= memshoft "606") (progn (setq flange 60) (setq bm 35) (setq th 6)))
(if (= memshoft "654") (progn (setq flange 65) (setq bm 36) (setq th 4)))
(if (= memshoft "655") (progn (setq flange 65) (setq bm 36) (setq th 5)))
(if (= memshoft "656") (progn (setq flange 65) (setq bm 36) (setq th 6)))
(if (= memshoft "705") (progn (setq flange 70) (setq bm 41) (setq th 5)))
(if (= memshoft "706") (progn (setq flange 70) (setq bm 41) (setq th 6)))
(if (= memshoft "755") (progn (setq flange 75) (setq bm 44) (setq th 5)))
(if (= memshoft "756") (progn (setq flange 75) (setq bm 44) (setq th 6)))
(if (= memshoft "806") (progn (setq flange 80) (setq bm 46) (setq th 6)))
(if (= memshoft "808") (progn (setq flange 80) (setq bm 46) (setq th 8)))
(if (= memshoft "906") (progn (setq flange 90) (setq bm 51) (setq th 6)))
(if (= memshoft "907") (progn (setq flange 90) (setq bm 51) (setq th 7)))
(if (= memshoft "908") (progn (setq flange 90) (setq bm 51) (setq th 8)))
(if (= memshoft "1006") (progn (setq flange 100) (setq bm 56) (setq th 6)))
(if (= memshoft "1007") (progn (setq flange 100) (setq bm 56) (setq th 7)))
(if (= memshoft "1008") (progn (setq flange 100) (setq bm 56) (setq th 8)))
(if (= memshoft "10010") (progn (setq flange 100) (setq bm 58) (setq th 10)))
(if (= memshoft "1108") (progn (setq flange 110) (setq bm 61) (setq th 8)))
(if (= memshoft "11010") (progn (setq flange 110) (setq bm 61) (setq th 10)))
(if (= memshoft "1208") (progn (setq flange 120) (setq bm 70) (setq th 8)))
(if (= memshoft "12010") (progn (setq flange 120) (setq bm 70) (setq th 10)))
(if (= memshoft "13010") (progn (setq flange 130) (setq bm 78) (setq th 10)))
(if (= memshoft "13012") (progn (setq flange 130) (setq bm 78) (setq th 12)))
(if (= memshoft "15012") (progn (setq flange 150) (setq bm 88) (setq th 12)))	
(if ( and (> memshoft "450") (< memshoft "509")) 
(progn 
(setq aclamp_cleat_width 75) (setq aclamp_cleat_length 60) (setq aclamp_cleat_bk 54) (setq aclamp_cleat_thk 5)
(setq aclamp_plate1_width 146) (setq aclamp_plate1_length 60) (setq aclamp_plate1_thk 6) 
(setq aclamp_plate2_width 85) (setq aclamp_plate2_length 60) (setq aclamp_plate2_thk 6)))
(if ( and (> memshoft "550") (< memshoft "609")) 
(progn (setq aclamp_cleat_width 90) (setq aclamp_cleat_length 60) (setq aclamp_cleat_bk 64) (setq aclamp_cleat_thk 5)
(setq aclamp_plate1_width 156) (setq aclamp_plate1_length 60) (setq aclamp_plate1_thk 6) 
(setq aclamp_plate2_width 95) (setq aclamp_plate2_length 60) (setq aclamp_plate2_thk 6)))
(if ( and (> memshoft "650") (< memshoft "759")) 
(progn (setq aclamp_cleat_width 100) (setq aclamp_cleat_length 60) (setq aclamp_cleat_bk 79) (setq aclamp_cleat_thk 6)
(setq aclamp_plate1_width 171) (setq aclamp_plate1_length 60) (setq aclamp_plate1_thk 6) 
(setq aclamp_plate2_width 110) (setq aclamp_plate2_length 60) (setq aclamp_plate2_thk 6)))
(if ( and (> memshoft "800") (< memshoft "910")) 
(progn (setq aclamp_cleat_width 130) (setq aclamp_cleat_length 60) (setq aclamp_cleat_bk 93) (setq aclamp_cleat_thk 10)
(setq aclamp_plate1_width 186) (setq aclamp_plate1_length 60) (setq aclamp_plate1_thk 6) 
(setq aclamp_plate2_width 125) (setq aclamp_plate2_length 60) (setq aclamp_plate2_thk 6)))
(if ( and (> memshoft "1004") (< memshoft "10010")) 
(progn (setq aclamp_cleat_width 130) (setq aclamp_cleat_length 60) (setq aclamp_cleat_bk 99) (setq aclamp_cleat_thk 10)
(setq aclamp_plate1_width 196) (setq aclamp_plate1_length 60) (setq aclamp_plate1_thk 6) 
(setq aclamp_plate2_width 135) (setq aclamp_plate2_length 60) (setq aclamp_plate2_thk 6)))
(setq temp1 (- aclamp_plate1_width shearedge (+ aclamp_plate2_thk welding_length)))
(setq tempcal1 (- temp1 bm))
(setq tempcal2 (abs (/ tempcal1 (sin ang4))))
(setq tempcal3 (sqrt (- (* tempcal2 tempcal2) (* tempcal1 tempcal1)))) ;shifting length
(setq pp50t (polar aclampin ang2 tempcal3))
(setq pp50 (polar pp50t ang1 bm))
(setq pp51 (polar pp50 ang3 (- aclamp_plate1_width (+ aclamp_plate2_thk welding_length))))
(setq pp52 (polar pp51 ang2 (/ aclamp_plate1_length 2)))
(setq pp53 (polar pp52 ang1 1)) 
(setq pp54 (polar pp52 ang1 aclamp_plate1_width))
(setq pp55 (polar pp54 ang aclamp_plate1_length))
(setq pp56 (polar pp55 ang3 aclamp_plate1_width))
(setq pp57 (polar pp56 ang3 1))
(setq pp58 (polar pp54 ang3 (+ aclamp_plate2_thk welding_length th 1)))
(setq pp59 (polar pp55 ang3 (+ aclamp_plate2_thk welding_length th 1)))
(setq pp60 (polar pp59 ang3 aclamp_cleat_thk))
(setq pp61 (polar pp58 ang3 aclamp_cleat_thk))
(setq pp62 (polar pp50 ang2 (/ aclamp_cleat_length 2)))
(setq pp63 (polar pp62 ang1 aclamp_plate2_thk))
(setq pp64 (polar pp63 ang aclamp_plate2_length))
(setq pp65 (polar pp64 ang3 aclamp_plate2_thk))
(setq pp66 (polar pp51 ang1 (+ aclamp_plate1_width 1))) ;center line
(setq pp67 (polar pp52 ang1 shearedge)) ;bottam center line left
(setq pp68 (polar pp57 ang1 shearedge)) ;bottam center line right
(setq pp69 (polar pp54 ang3 shearedge)) ;top center line left
(setq pp70 (polar pp55 ang3 shearedge)) ;top center line right
(setq pp71 (inters pp51 pp66 pp69 pp70)) ;heel side hole location
(setq pp72 (inters pp51 pp66 pp67 pp68)) ;other side hole location
;(command "line" pp52 pp54 pp55 pp57 pp52 "" "line" pp53 pp56 "" "line" pp58 pp59 "" "line" pp60 pp61 "")
;(command "PLINE" pp62 "W" "0" "" pp62 pp63 pp64 pp65 pp62 "" )
;(command "line" pp51 pp66 "" "line" pp67 pp68 "" "line" pp69 pp70 "")
;(command "_insert" "16c" pp71 "" "" "") 
;(command "_insert" "16c" pp72 "" "" "") 
(setq angle_clamp_hole pp72)
)
(defun Number_Round (ImpVal To_Val) ;to round off the value
  (setq To_Val (abs To_Val))
  (*
    To_Val
    (fix
      (/
        (
          (if (minusp ImpVal)
            -
            +
          )
          ImpVal
          (* To_Val 0.5)
        )
        To_Val
      )
    )
  )
)
(defun hip_eqfaces()
(defun rtd(x) (* (/ 180 pi) x))
(defun dtr(x) (* (/ pi 180.0) x))
(defun asin (z /) (atan z (sqrt (- 1.0 (* z z)))))
(setq hip_mem (getreal "Enter the number of hip/plan members <0/1/2>:: "))

(if (= hip_mem 1) (progn
(setq hip_mem1_hight (getreal "Enter the hip member vertical hight::"))
(initget 1 "A P")
(setq hip_mem1_type (getreal "Enter the type of new hip member<A/P>:"))
(setq hip_mem1_size (getstring "Enter the new hip member size:"))
(setq st1_angle1 (- 180 (rtd (angle structending structstarting ))))
(setq st1_angle2 (- 180 (+ st1_angle1 90)))
(setq a1 (/ hip_mem1_hight (sin (dtr st1_angle1))))
(setq b1 (/ (* hip_mem1_hight (sin (dtr st1_angle2))) (sin (dtr st1_angle1))))
(setq x1 (- (+ exst1_x (- (* (/ (sin (dtr st1_angle2)) (sin (dtr st1_angle1)) ) 135) 50)) (- b1 (/ bm (sin (dtr st1_angle1))))))
(setq z (sqrt (- (+ (* x1 x1) (* x1 x1)) (* 2 x1 x1 (cos (dtr twost_ang))))))
(setq hip_plan_ang1 (rtd (asin (/ (* x1 (sin (dtr twost_ang))) z))))
(setq hip_plan_ang2 (- 180 (+ hip_plan_ang1 (dtr twost_ang))))
(setq memshoft hip_mem1_size)
(if (= exst1_ty "A") (progn (setq hip_memlen1 (Number_Round (- z (* (+ (* (+ structsize 10) 0.707) 40) 2) ) 1)) (angmember) ))
(if (= exst1_ty "P") (progn (setq hip_memlen1 (Number_Round (- z (* (+ (/ structsize 2) 75) 2)) 1)) (pipemember) ))
))

(if (= hip_mem 2) (progn
(setq hip_mem1_hight (getreal "Enter the first hip member vertical hight::"))
(initget 1 "A P")
(setq hip_mem1_type (getreal "Enter the first hip member type<A/P>::"))
(setq hip_mem1_size (getstring "Enter the first hip member size::"))

(setq st1_angle1 (- 180 (rtd (angle structending structstarting ))))
(setq st1_angle2 (- 180 (+ st1_angle1 90)))
(setq a1 (/ hip_mem1_hight (sin (dtr st1_angle1))))
(setq b1 (/ (* hip_mem1_hight (sin (dtr st1_angle2))) (sin (dtr st1_angle1))))
(setq x1 (- (+ exst1_x (- (* (/ (sin (dtr st1_angle2)) (sin (dtr st1_angle1)) ) 135) 50)) (- b1 (/ bm (sin (dtr st1_angle1))))))
(setq z (sqrt (- (+ (* x1 x1) (* x1 x1)) (* 2 x1 x1 (cos (dtr twost_ang))))))
(setq hip_plan_ang1 (rtd (asin (/ (* x1 (sin (dtr twost_ang))) z))))
(setq hip_plan_ang2 (- 180 (+ hip_plan_ang1 (dtr twost_ang))))
(setq memshoft hip_mem1_size)
(if (= exst1_ty "A") (progn (setq hip_memlen1 (Number_Round (- z (* (+ (* (+ structsize 10) 0.707) 40) 2) ) 1))))
(if (= exst1_ty "P") (progn (setq hip_memlen1 (Number_Round (- z (* (+ (/ structsize 2) 75) 2)) 1))))

(setq hip_mem2_hight (getreal "Enter the secound hip member vertical hight::"))
(initget 1 "A P")
(setq hip_mem2_type (getreal "Enter the secound hip member type</A/P>::"))
(setq hip_mem2_size (getstring "Enter the secound hip member size::"))

(setq st1_angle1 (- 180 (rtd (angle structending  structstarting))))
(setq st1_angle2 (- 180 (+ st1_angle1 90)))
(setq a1 (/ hip_mem2_hight (sin (dtr st1_angle1))))
(setq b1 (/ (* hip_mem2_hight (sin (dtr st1_angle2))) (sin (dtr st1_angle1))))
(setq x1 (- (+ exst1_x (- (* (/ (sin (dtr st1_angle2)) (sin (dtr st1_angle1)) ) 135) 50)) (- b1 (/ bm (sin (dtr st1_angle1))))))
(setq z (sqrt (- (+ (* x1 x1) (* x1 x1)) (* 2 x1 x1 (cos (dtr twost_ang))))))
(setq hip_plan_ang1 (rtd (asin (/ (* x1 (sin (dtr twost_ang))) z))))
(setq hip_plan_ang2 (- 180 (+ hip_plan_ang1 (dtr twost_ang))))
(setq memshoft hip_mem2_size)
(if (= exst1_ty "A") (progn (setq hip_memlen2 (Number_Round (- z (* (+ (* (+ structsize 10) 0.707) 40) 2) ) 1)) (angmember) ))
(if (= exst1_ty "P") (progn (setq hip_memlen2 (Number_Round (- z (* (+ (/ structsize 2) 75) 2)) 1)) (pipemember) ))
))
)
(defun hip_twoDfaces()
(defun rtd(x) (* (/ 180 pi) x))
(defun dtr(x) (* (/ pi 180.0) x))
(defun asin (z /) (atan z (sqrt (- 1.0 (* z z)))))

(setq face1_hip f1)
(setq face2_hip f2)
(setq hip_mem1_hight h)
(setq angle_b_structs angle)
(setq st1_angle1 (abs (- 180 (rtd (angle structstarting1 structending1 )))))
(setq st1_angle2 (abs (- 180 (+ st1_angle1 90))))
(setq st2_angle1 (abs (- 180 (rtd (angle structstarting2 structending2 )))))
(setq st2_angle2 (abs (- 180 (+ st2_angle1 90))))
(setq a1 (/ h (sin (dtr st1_angle1))))
(setq b1 (/ (* h (sin (dtr st1_angle2))) (sin (dtr st1_angle1))))
(setq x1 (- face1_hip b1))
(setq a2 (/ h (sin (dtr st2_angle1))))
(setq b2 (/ (* h (sin (dtr st2_angle2))) (sin (dtr st2_angle1))))
(setq x2 (- face2_hip b2))
(setq z (sqrt (- (+ (* x1 x1) (* x2 x2)) (* 2 x1 x2 (cos (dtr angle_b_structs))))))
(setq hip_plan_ang1 (asin (/ (* x1 (sin (dtr angle_b_structs))) z)))
(setq hip_plan_ang2 (- 180 (+ hip_plan_ang1 (dtr angle_b_structs))))
(if (= structty "A") (progn (setq memlen1 (+ (* (+ structsize1 10) 0.707) 40)) (setq memlen2 (+ (* (+ structsize2 10) 0.707) 40)) (setq memlen (+ memlen1 memlen2) ) ))
(if (= structty "P") (progn (setq memlen (- z (* (+ (/ structsize 2) 45) 2)))))

)
;------------------------------------------------------------------------------
;                   L A Y E R   C R E A T I O N
;------------------------------------------------------------------------------
(if (= venki nil) (progn
(command "layer" "m" "mem" "c" "4" "" "")
(command "layer" "m" "des" "c" "1" "" "")
(command "layer" "m" "gus" "c" "3" "" "")
(command "layer" "m" "con" "c" "1" "" "")
(command "layer" "m" "dim" "c" "5" "" "")
(command "layer" "m" "len" "c" "2" "" "")
(command "layer" "m" "int" "c" "7" "" "")
(command "layer" "m" "bolt" "c" "6" "" "")
(command "layer" "m" "bdes" "c" "6" "" "")
(command "layer" "m" "das" "c" "1" "" "lt" "dashed" "" "")
(command "layer" "m" "hid" "c" "1" "" "lt" "hidden" "" "")
(command "layer" "m" "cen" "c" "2" "" "lt" "center" "" "")
(command "layer" "s" "0" "")
(defun c:mem  () (command "change" pause "" "p" "la" "mem" "lt" "bylayer" "c" "bylayer" ""))
(defun c:das  () (command "change" pause "" "p" "la" "das" "lt" "bylayer" "c" "bylayer" ""))
(defun c:cen  () (command "change" pause "" "p" "la" "cen" "lt" "bylayer" "c" "bylayer" ""))
(defun c:des  () (command "change" pause "" "p" "la" "des" "lt" "bylayer" "c" "bylayer" ""))
(defun c:gus  () (command "change" pause "" "p" "la" "gus" "lt" "bylayer" "c" "bylayer" ""))
(defun c:con  () (command "change" pause "" "p" "la" "con" "lt" "bylayer" "c" "bylayer" ""))
(defun c:de   () (command "change" pause "" "p" "la" "dim" "lt" "bylayer" "c" "bylayer" ""))
(defun c:bot  () (command "change" pause "" "p" "la" "bolt" "lt" "bylayer" "c" "bylayer" ""))
(defun c:FAB  () (command "change" pause "" "p" "la" "FAB" "lt" "bylayer" "c" "bylayer" "")) ;yellow continus
(defun c:hid  () (command "change" pause "" "p" "la" "das" "lt" "hidden" "" )) ;hidden
(defun c:bl   () (command "change" pause "" "p" "la" "das" "lt" "dashdot" "" ))
(defun c:bdes () (command "change" pause "" "p" "la" "bdes" "lt" "bylayer" "" )) ;majentha continus

(defun mem() (command "change" pause "" "p" "la" "mem" "lt" "bylayer" "c" "bylayer" ""))
(defun das() (command "change" pause "" "p" "la" "das" "lt" "bylayer" "c" "bylayer" ""))
(defun cen() (command "change" pause "" "p" "la" "cen" "lt" "bylayer" "c" "bylayer" ""))
(defun des() (command "change" pause "" "p" "la" "des" "lt" "bylayer" "c" "bylayer" ""))
(defun gus() (command "change" pause "" "p" "la" "gus" "lt" "bylayer" "c" "bylayer" ""))
(defun con() (command "change" pause "" "p" "la" "con" "lt" "bylayer" "c" "bylayer" ""))
(defun de() (command "change" pause "" "p" "la" "dim" "lt" "bylayer" "c" "bylayer" ""))
(defun bot() (command "change" pause "" "p" "la" "bolt" "lt" "bylayer" "c" "bylayer" ""))
(defun FAB() (command "change" pause "" "p" "la" "FAB" "lt" "bylayer" "c" "bylayer" "")) ;yellow continus
(defun hid() (command "change" pause "" "p" "la" "das" "lt" "hidden" "" )) ;hidden
(defun bl() (command "change" pause "" "p" "la" "das" "lt" "dashdot" "" ))
(defun bdes() (command "change" pause "" "p" "la" "bdes" "lt" "bylayer" "" )) ;majentha continus
(setq sf 15)
(command "dimscale" sf)
(command "style" "arial" "arial" (* 4.0 sf) "" "" "" "")
(command "style" "standard" "simplex" (* 2.0 sf) "0.8" "" "" "" "")
(command "style" "SHX_style" "simplex" (* 2.5 sf) "0.8" "" "" "" "")

(command "ltscale" (* 5.0 sf))
(command "dimasz" 2.5)
(command "dimtsz" 0.5)
(command "dimtxt" 2.5)
(command "dimexe" 1)
(command "dimexo" 1)
(command "dimlwd" 9)
(command "dimlwe" 9)
(command "dimgap" 1)
(command "dimtad" 1)
(command "dimtofl" 1)
(command "dimtih" 0)
(command "dimtoh" 0)
(command "dimlwd" 9) 
(command "dimlwe" 9)

(command "_insert" "c:/detail/blocks/ALBABTAIN BLOCKS/15SCALEBLOCKS" "@" "" "" "")
(command "_insert" "c:/detail/POLEDATA" "@" "" "" "" )
;(defun c:newp() (setq IP nil) (setq ibp nil))
(setq venki "1")
))
(defun spd_drawings()
(if (= designtype "SPD") (progn
	(if (or (= design_name "IN-6.0M-RIT-120-CNTR-MP") (= design_name "IN-6.0m-RIT-120-CNTR-MP") (= design_name "In-6.0m-rit-120-cntr-mp")) (progn 
		(if (and (= newstudnum 1) (= newstud1_memty "P") (= newst_mem_onpole 0) (= newst_mem_onstruct 0) (= hip_mem 0) ) (progn 
				
				(if (= ibp nil) (setq ibp '(0.0 0.0 0.0)))
				(if (= polenumber nil) (setq polenumber 1) (setq polenumber (1+ polenumber)))
				(command "_insert" "c:/detail/POLEDATA/IN-6.0M-RIT-120-CNTR-MP_P_SOL1" ibp "" "" "" )
				
				(setq dm1t (polar ibp (DTR 0.0) 1178.66))  (setq dm1 (polar dm1t (DTR 90.0) 2079.96))  ;b1
				(setq dm2t (polar ibp (DTR 0.0) 1103.43))  (setq dm2 (polar dm2t (DTR 90.0) 4891.84))  ;b2
				(setq dm3t (polar ibp (DTR 0.0) 1178.66))  (setq dm3 (polar dm3t (DTR 90.0) 5829.96))  ;b3
				(setq dm4t (polar ibp (DTR 0.0) 1859.45))  (setq dm4 (polar dm4t (DTR 90.0) 1877.72))  ;b4
				(setq dm5t (polar ibp (DTR 0.0) 2978.2))  (setq dm5 (polar dm5t (DTR 90.0) 1877.72))  ;b5
				(setq dm6t (polar ibp (DTR 0.0) 3342.48))  (setq dm6 (polar dm6t (DTR 90.0) 2000.29))  ;b6
				(setq dm7t (polar ibp (DTR 0.0) 2148.84))  (setq dm7 (polar dm7t (DTR 90.0) 1508.42))  ;b7
				(setq dm8t (polar ibp (DTR 0.0) 3903.34))  (setq dm8 (polar dm8t (DTR 90.0) 989.86))  ;b8
				(setq dm9t (polar ibp (DTR 0.0) 4653.34))  (setq dm9 (polar dm9t (DTR 90.0) 988.36))  ;b9
				(setq dm10t (polar ibp (DTR 0.0) 6510.54))  (setq dm10 (polar dm10t (DTR 90.0) 6140))  ;b10
				(setq dm11t (polar ibp (DTR 0.0) 5289.27))  (setq dm11 (polar dm11t (DTR 90.0) 6023.39))  ;b11
				(setq dm12t (polar ibp (DTR 0.0) 5372.67))  (setq dm12 (polar dm12t (DTR 90.0) 6023.39))  ;b12
				(setq dm13t (polar ibp (DTR 0.0) 5461.06))  (setq dm13 (polar dm13t (DTR 90.0) 6023.39))  ;b13
				(setq dm14t (polar ibp (DTR 0.0) 5289.72))  (setq dm14 (polar dm14t (DTR 90.0) 5948.39))  ;b14
				(setq dm15t (polar ibp (DTR 0.0) 5289.27))  (setq dm15 (polar dm15t (DTR 90.0) 5880.29))  ;b15
				(setq dm16t (polar ibp (DTR 0.0) 5425.92))  (setq dm16 (polar dm16t (DTR 90.0) 4223.29))  ;b16
				(setq dm17t (polar ibp (DTR 0.0) 4941.95))  (setq dm17 (polar dm17t (DTR 90.0) 4126.69))  ;b17
				(setq dm18t (polar ibp (DTR 0.0) 5909.89))  (setq dm18 (polar dm18t (DTR 90.0) 4126.69))  ;b18
				(setq dm19t (polar ibp (DTR 0.0) 5026.74))  (setq dm19 (polar dm19t (DTR 90.0) 3814.82))  ;b19
				(setq dm20t (polar ibp (DTR 0.0) 5128.21))  (setq dm20 (polar dm20t (DTR 90.0) 2702.22))  ;b20
				(setq dm21t (polar ibp (DTR 0.0) 7901.03))  (setq dm21 (polar dm21t (DTR 90.0) 2717.21))  ;b21
				(setq dm22t (polar ibp (DTR 0.0) 8013.51))  (setq dm22 (polar dm22t (DTR 90.0) 2717.21))  ;b22
				(setq dm23t (polar ibp (DTR 0.0) 8148.23))  (setq dm23 (polar dm23t (DTR 90.0) 2717.49))  ;b23
				(setq dm24t (polar ibp (DTR 0.0) 7818.23))  (setq dm24 (polar dm24t (DTR 90.0) 2634.69))  ;b24
				(setq dm25t (polar ibp (DTR 0.0) 7818.23))  (setq dm25 (polar dm25t (DTR 90.0) 2522.21))  ;b25
				(setq dm26t (polar ibp (DTR 0.0) 7818.23))  (setq dm26 (polar dm26t (DTR 90.0) 2372.21))  ;b26
				(setq dm27t (polar ibp (DTR 0.0) 8750))  (setq dm27 (polar dm27t (DTR 90.0) 6140))  ;b27

				(command "dim1" "ver" dm1 dm2 (polar dm1 (DTR 180.0) 190) (fix newstud1_y1))
				(command "dim1" "ver" dm2 dm3 (polar dm1 (DTR 180.0) 190) (fix (- joint_0 newstud1_y1)))
				(command "dim1" "hor" dm4 dm5 (polar dm4 (DTR 270.0) 160) (fix newstud1_x1 ))
				(command "dim1" "hor" dm8 dm9 (polar dm8 (DTR 270.0) 170) (fix newstud1_x1))
				
				(command "style" "TRB" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" dm6 "0" (strcat (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdht 2 0 )))
				(command "TEXT" dm7 "0" (strcat "(POLE-" (itoa polenumber) ")"))
				(command "_insert" "c:/detail/POLEDATA/POLE-L4_120x3" dm10 "" "" "" )				

				(command "dim1" "hor" dm11 dm12 (polar dm12 (DTR 90.0) 42.5) newstud1_platex2)
				(command "dim1" "hor" dm12 dm13 (polar dm12 (DTR 90.0) 42.5) newstud1_platex1)
				(command "dim1" "ver" dm11 dm14 (polar dm14 (DTR 180.0) 47) newstud1_platey2)
				(command "dim1" "ver" dm14 dm15 (polar dm14 (DTR 180.0) 47) newstud1_platey1)
								
				(if (= newst1_size 48.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_48.3_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_48.3_2BOLTS" dm20 "" "" "" )))
				(if (= newst1_size 60.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_60.3_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_60.3_2BOLTS" dm20 "" "" "" )))
				(if (= newst1_size 76.1) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_76.1_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_76.1_2BOLTS" dm20 "" "" "" )))
				(if (= newst1_size 88.9) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_88.9_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_88.9_2BOLTS" dm20 "" "" "" )))
				(if (= newst1_size 101.6) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_101.6_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_101.6_2BOLTS" dm20 "" "" "" )))
				(if (= newst1_size 114.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_114.3_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_114.3_2BOLTS" dm20 "" "" "" )))
				(if (= newst1_size 139.7) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_139.7_1" dm16 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_139.7_2BOLTS" dm20 "" "" "" )))
	
				(command "dim1" "hor" dm17 dm18 (polar dm17 (DTR 270.0) 181) (Number_Round (- newstud1_memlen (+ newst1_sizename2 35) (+ newst1_sizename2 35) 80) 1))
				(command "_insert" "f3x" dm19 "0.0394" "0.0394" "" "N3" (strcat newstud1_memname ".." (itoa (Number_Round (+ newstud1_memlen 50) 1)) " Lg"))
				(command "line" (polar dm19 (DTR 7.0) 119) (polar (polar dm19 (DTR 7.0) 119) (DTR 0.0) 640) "") 
				(command "dim1" "hor" dm21 dm22 (polar dm22 (DTR 90.0) 79) newstud1_platex1)
				(command "dim1" "hor" dm22 dm23 (polar dm22 (DTR 90.0) 79) newstud1_platex2)
				(command "dim1" "ver" dm24 dm25 (polar dm25 (DTR 180.0) 79) newstud1_platey1)
				(command "dim1" "ver" dm25 dm26 (polar dm25 (DTR 180.0) 79) newstud1_platey2)
				;BOM
				(setq ibom dm27)
				(setq N3 (* (/ (Number_Round (+ newstud1_memlen 50) 1) 1000.0) newstud1_blockwt 3))
				(setq concrete_vol (* (/ newstud1_pdwt 1000.0) (/ newstud1_pdwt 1000.0) (/ newstud1_pdht 1000.0) 3))
				(setq total_mem_weight (+ N3 21.55))
				(setq total_weight (+ total_mem_weight 3.2))
				(command "_insert" "c:/detail/POLEDATA/IN-6.0M-RIT-120-CNTR-MP_P_SOL1_bom" ibom "" "" "" )
				(setq bom1t (polar ibom (DTR 180.0) 1609.7))  (setq bom1 (polar bom1t (DTR 270.0) 469.67))  ;aa1
				(setq bom2t (polar ibom (DTR 180.0) 1249.11))  (setq bom2 (polar bom2t (DTR 270.0) 469.67))  ;aa2
				(setq bom3t (polar ibom (DTR 180.0) 858.41))  (setq bom3 (polar bom3t (DTR 270.0) 472.1))  ;aa3
				(setq bom4t (polar ibom (DTR 180.0) 511.74))  (setq bom4 (polar bom4t (DTR 270.0) 472.1))  ;aa4
				(setq bom5t (polar ibom (DTR 180.0) 888.35))  (setq bom5 (polar bom5t (DTR 270.0) 530.96))  ;aa5
				(setq bom6t (polar ibom (DTR 180.0) 541.68))  (setq bom6 (polar bom6t (DTR 270.0) 530.96))  ;aa6
				(setq bom7t (polar ibom (DTR 180.0) 899.71))  (setq bom7 (polar bom7t (DTR 270.0) 1117.95))  ;aa7
				(setq bom8t (polar ibom (DTR 180.0) 553.04))  (setq bom8 (polar bom8t (DTR 270.0) 1117.95))  ;aa8
				(setq bom9t (polar ibom (DTR 180.0) 899.71))  (setq bom9 (polar bom9t (DTR 270.0) 1245.41))  ;aa9
				(setq bom10t (polar ibom (DTR 180.0) 553.04))  (setq bom10 (polar bom10t (DTR 270.0) 1245.41))  ;aa10
				(setq bom11t (polar ibom (DTR 180.0) 674.44))  (setq bom11 (polar bom11t (DTR 270.0) 1303.22))  ;aa11
				(command "style" "TRB" "Trebuchet MS" (* 2.0 sf) "1" "" "" "")
				(command "TEXT" bom1 "0" newstud1_memname)
				(command "TEXT" bom2 "0" (itoa (Number_Round (+ newstud1_memlen 50) 1)))
				(command "TEXT" bom3 "0" (rtos N3 2 2))
				(command "TEXT" bom4 "0" (rtos (* 1.035 N3 ) 2 2))
				(command "TEXT" bom11 "0" (rtos concrete_vol 2 3))	
				(command "style" "WMF-Trebuchet MS0" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" bom5 "0" (rtos total_mem_weight 2 2))
				(command "TEXT" bom6 "0" (rtos (* total_mem_weight 1.035) 2 2))
				(command "TEXT" bom7 "0" (rtos total_mem_weight 2 2))
				(command "TEXT" bom8 "0" (rtos (* total_mem_weight 1.035) 2 2))
				(command "TEXT" bom9 "0" (rtos total_weight 2 2))
				(command "TEXT" bom10 "0" (rtos (* total_weight 1.035) 2 2))
								
				(setq next1 (polar ibp (dtr 0.0) 9900))
				(setq ibp next1)
		))		
		
		(if (and (= newstudnum 1) (= newstud1_memty "P") (= newst_mem_onpole 1) (= newst_mem_onstruct 1) (= newst1_newmem1_type "P") (= (fix hip_mem) 0)) (progn
				(if (= ibp nil) (setq ibp '(0.0 0.0 0.0)))
				(if (= polenumber nil) (setq polenumber 1) (setq polenumber (1+ polenumber)))
				(command "_insert" "c:/detail/bolckss/IN-6.0M-RIT-120-CNTR-MP_P_SOL2" ibp "" "" "" )
				
				(setq dm1t (polar ibp (DTR 0.0) 772.69))  (setq dm1 (polar dm1t (DTR 90.0) 2079.96))  ;b1
				(setq dm2t (polar ibp (DTR 0.0) 772.69))  (setq dm2 (polar dm2t (DTR 90.0) 3476.47))  ;b2
				(setq dm3t (polar ibp (DTR 0.0) 750.06))  (setq dm3 (polar dm3t (DTR 90.0) 4891.84))  ;b3
				(setq dm4t (polar ibp (DTR 0.0) 825.3))  (setq dm4 (polar dm4t (DTR 90.0) 5829.96))  ;b4
				(setq dm5t (polar ibp (DTR 0.0) 1506.08))  (setq dm5 (polar dm5t (DTR 90.0) 1877.72))  ;b5
				(setq dm6t (polar ibp (DTR 0.0) 2631.08))  (setq dm6 (polar dm6t (DTR 90.0) 1877.72))  ;b6
				(setq dm7t (polar ibp (DTR 0.0) 2989.11))  (setq dm7 (polar dm7t (DTR 90.0) 1991.85))  ;b7
				(setq dm8t (polar ibp (DTR 0.0) 3088.76))  (setq dm8 (polar dm8t (DTR 90.0) 923.03))  ;b8
				(setq dm9t (polar ibp (DTR 0.0) 3839.4))  (setq dm9 (polar dm9t (DTR 90.0) 921.81))  ;b9
				(setq dm10t (polar ibp (DTR 0.0) 6525.48))  (setq dm10 (polar dm10t (DTR 90.0) 6150))  ;b10
				(setq dm11t (polar ibp (DTR 0.0) 6525.48))  (setq dm11 (polar dm11t (DTR 90.0) 4565.77))  ;b11
				(setq dm12t (polar ibp (DTR 0.0) 5304.21))  (setq dm12 (polar dm12t (DTR 90.0) 6033.39))  ;b12
				(setq dm13t (polar ibp (DTR 0.0) 5387.61))  (setq dm13 (polar dm13t (DTR 90.0) 6026.48))  ;b13
				(setq dm14t (polar ibp (DTR 0.0) 5476))  (setq dm14 (polar dm14t (DTR 90.0) 6033.39))  ;b14
				(setq dm15t (polar ibp (DTR 0.0) 5304.66))  (setq dm15 (polar dm15t (DTR 90.0) 5958.39))  ;b15
				(setq dm16t (polar ibp (DTR 0.0) 5304.21))  (setq dm16 (polar dm16t (DTR 90.0) 5890.29))  ;b16
				(setq dm17t (polar ibp (DTR 0.0) 5467.83))  (setq dm17 (polar dm17t (DTR 90.0) 2720.12))  ;b17
				(setq dm18t (polar ibp (DTR 0.0) 4983.86))  (setq dm18 (polar dm18t (DTR 90.0) 2623.52))  ;b18
				(setq dm19t (polar ibp (DTR 0.0) 5564.59))  (setq dm19 (polar dm19t (DTR 90.0) 2623.51))  ;b19
				(setq dm20t (polar ibp (DTR 0.0) 5951.8))  (setq dm20 (polar dm20t (DTR 90.0) 2623.52))  ;b20
				(setq dm21t (polar ibp (DTR 0.0) 5068.65))  (setq dm21 (polar dm21t (DTR 90.0) 2304.19))  ;b21
				(setq dm22t (polar ibp (DTR 0.0) 5167.15))  (setq dm22 (polar dm22t (DTR 90.0) 2055.09))  ;b22
				(setq dm23t (polar ibp (DTR 0.0) 4821.81))  (setq dm23 (polar dm23t (DTR 90.0) 1958.49))  ;b23
				(setq dm24t (polar ibp (DTR 0.0) 5512.49))  (setq dm24 (polar dm24t (DTR 90.0) 1958.49))  ;b24
				(setq dm25t (polar ibp (DTR 0.0) 4777.97))  (setq dm25 (polar dm25t (DTR 90.0) 1646.62))  ;b25
				(setq dm26t (polar ibp (DTR 0.0) 6261.01))  (setq dm26 (polar dm26t (DTR 90.0) 2073.33))  ;b26
				(setq dm27t (polar ibp (DTR 0.0) 6337.53))  (setq dm27 (polar dm27t (DTR 90.0) 2073.33))  ;b27
				(setq dm28t (polar ibp (DTR 0.0) 6000.95))  (setq dm28 (polar dm28t (DTR 90.0) 1873.33))  ;b28
				(setq dm29t (polar ibp (DTR 0.0) 6337.53))  (setq dm29 (polar dm29t (DTR 90.0) 1873.33))  ;b29
				(setq dm30t (polar ibp (DTR 0.0) 5895.65))  (setq dm30 (polar dm30t (DTR 90.0) 1651.39))  ;b30
				(setq dm31t (polar ibp (DTR 0.0) 4575.84))  (setq dm31 (polar dm31t (DTR 90.0) 471.79))  ;b31
				(setq dm32t (polar ibp (DTR 0.0) 5585.57))  (setq dm32 (polar dm32t (DTR 90.0) 459.35))  ;b32
				(setq dm33t (polar ibp (DTR 0.0) 7911.03))  (setq dm33 (polar dm33t (DTR 90.0) 2527.33))  ;b33
				(setq dm34t (polar ibp (DTR 0.0) 8023.51))  (setq dm34 (polar dm34t (DTR 90.0) 2527.33))  ;b34
				(setq dm35t (polar ibp (DTR 0.0) 8158.23))  (setq dm35 (polar dm35t (DTR 90.0) 2527.62))  ;b35
				(setq dm36t (polar ibp (DTR 0.0) 7828.23))  (setq dm36 (polar dm36t (DTR 90.0) 2444.82))  ;b36
				(setq dm37t (polar ibp (DTR 0.0) 7828.23))  (setq dm37 (polar dm37t (DTR 90.0) 2332.33))  ;b37
				(setq dm38t (polar ibp (DTR 0.0) 7828.23))  (setq dm38 (polar dm38t (DTR 90.0) 2182.33))  ;b38
				(setq dm39t (polar ibp (DTR 0.0) 8756.64))  (setq dm39 (polar dm39t (DTR 90.0) 6150))  ;b39
				(setq dm40t (polar ibp (DTR 0.0) 2018))  (setq dm40 (polar dm40t (DTR 90.0) 1508.42))  ;b40
								
				(command "dim1" "ver" dm1 dm2 (polar dm1 (DTR 180.0) 190) (fix newst1_newmem1_y1))
				(command "dim1" "ver" dm2 dm3 (polar dm1 (DTR 180.0) 190) (fix (- newstud1_y1 newst1_newmem1_y1)))
				(command "dim1" "ver" dm3 dm4 (polar dm1 (DTR 180.0) 190) (fix (- joint_0 newstud1_y1)))
				(command "dim1" "hor" dm5 dm6 (polar dm5 (DTR 270.0) 160) (fix newstud1_x1 ))
				(command "dim1" "hor" dm8 dm9 (polar dm8 (DTR 270.0) 170) (fix newstud1_x1))
				
				(command "style" "TRB" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" dm7 "0" (strcat (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdht 2 0 )))
				(command "TEXT" dm40 "0" (strcat "(POLE-" (itoa polenumber) ")"))
				(command "_insert" "c:/detail/POLEDATA/POLE-L4_120x3" dm10 "" "" "" )				
				(command "_insert" "c:/detail/POLEDATA/POLE-L14_120x3" dm11 "" "" "" )				

				(command "dim1" "hor" dm12 dm13 (polar dm13 (DTR 90.0) 42.5) newstud1_platex2)
				(command "dim1" "hor" dm13 dm14 (polar dm13 (DTR 90.0) 42.5) newstud1_platex1)
				(command "dim1" "ver" dm12 dm15 (polar dm15 (DTR 180.0) 47) newstud1_platey2)
				(command "dim1" "ver" dm15 dm16 (polar dm15 (DTR 180.0) 47) newstud1_platey1)
								
				(if (= newst1_size 48.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_48.3_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_48.3_2BOLTS" dm31 "" "" "" )))
				(if (= newst1_size 60.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_60.3_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_60.3_2BOLTS" dm31 "" "" "" )))
				(if (= newst1_size 76.1) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_76.1_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_76.1_2BOLTS" dm31 "" "" "" )))
				(if (= newst1_size 88.9) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_88.9_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_88.9_2BOLTS" dm31 "" "" "" )))
				(if (= newst1_size 101.6) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_101.6_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_101.6_2BOLTS" dm31 "" "" "" )))
				(if (= newst1_size 114.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_114.3_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_114.3_2BOLTS" dm31 "" "" "" )))
				(if (= newst1_size 139.7) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_139.7_2" dm17 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_139.7_2BOLTS" dm31 "" "" "" )))
	
				(if (= newst1_newmem1_size 48.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_48.3" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_48.3_1BOLTS" dm32 "" "" "" )))
				(if (= newst1_newmem1_size 60.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_60.3" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_60.3_1BOLTS" dm32 "" "" "" )))
				(if (= newst1_newmem1_size 76.1) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_76.1" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_76.1_1BOLTS" dm32 "" "" "" )))
				(if (= newst1_newmem1_size 88.9) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_88.9" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_88.9_1BOLTS" dm32 "" "" "" )))
				(if (= newst1_newmem1_size 101.6) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_101.6" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_101.6_1BOLTS" dm32 "" "" "" )))
				(if (= newst1_newmem1_size 114.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_114.3" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_114.3_1BOLTS" dm32 "" "" "" )))
				(if (= newst1_newmem1_size 139.7) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_139.7" dm22 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_139.7_1BOLTS" dm32 "" "" "" )))
				
				(command "dim1" "hor" dm18 dm19 (polar dm19 (DTR 270.0) 181) (- newst1_joint2 (+ newst1_sizename2 35) 40))
				(command "dim1" "hor" dm19 dm20 (polar dm19 (DTR 270.0) 181) (- newst1_joint1 (+ newst1_sizename2 35) 40))
				(command "_insert" "f3x" dm21 "0.0394" "0.0394" "" "N3" (strcat newstud1_memname ".." (itoa (Number_Round (+ newstud1_memlen 50) 1)) " Lg"))
				(command "dim1" "hor" dm23 dm24 (polar dm23 (DTR 270.0) 181) (Number_Round (- newst1_newmem1_len (+ newst1_newmem1_sizename2 35) (+ newst1_newmem1_sizename2 35)) 1))
				(command "_insert" "f3x" dm25 "0.0394" "0.0394" "" "N4" (strcat newstud1_memname ".." (itoa (Number_Round (+ newst1_newmem1_len 50) 1)) " Lg"))
				(command "dim1" "hor" dm26 dm27 (polar dm27 (DTR 90.0) 71) newst1_newmem1_stx2)
				(command "dim1" "hor" dm28 dm29 (polar dm28 (DTR 270.0) 90) (Number_Round newst1_newmem1_stx1 1))
				(command "_insert" "f3x" dm30 "0.0394" "0.0394" "" "N5" (strcat "PLT" (itoa (fix clampstk)) "x50.." (itoa (Number_Round newst1_newmem1_stx1 1)) " Lg"))
				;(command "line" (polar dm19 (DTR 7.0) 119) (polar (polar dm19 (DTR 7.0) 119) (DTR 0.0) 640) "") 
				(command "dim1" "hor" dm33 dm34 (polar dm34 (DTR 90.0) 79) newstud1_platex1)
				(command "dim1" "hor" dm34 dm35 (polar dm34 (DTR 90.0) 79) newstud1_platex2)
				(command "dim1" "ver" dm36 dm37 (polar dm37 (DTR 180.0) 79) newstud1_platey1)
				(command "dim1" "ver" dm37 dm38 (polar dm37 (DTR 180.0) 79) newstud1_platey2)
				
				;BOM
				(setq ibom dm39)
				(setq N3 (* (/ (Number_Round (+ newstud1_memlen 50) 1) 1000.0) newstud1_blockwt 3))
				(setq N4 (* (/ (Number_Round (+ newst1_newmem1_len 50) 1) 1000.0) newst1_newmem1_blockwt 3))
				(setq N5 (* (/ clampstk 1000.0 )(/ 50 1000.0) (/ (Number_Round newst1_newmem1_stx1 1) 1000.0) 7850.0 3))
				
				(setq concrete_vol (* (/ newstud1_pdwt 1000.0) (/ newstud1_pdwt 1000.0) (/ newstud1_pdht 1000.0) 3))
				(setq total_mem_weight (+ N3 N4 N5 27.43))
				(setq total_weight (+ total_mem_weight 5.32))
				(command "_insert" "c:/detail/POLEDATA/IN-6.0M-RIT-120-CNTR-MP_P_SOL2_bom" ibom "" "" "" )
				
				(setq bom1t (polar ibom (DTR 180.0) 1678.94)) (setq bom1 (polar bom1t (DTR 270.0) 673.09)) ;aa1
				(setq bom2t (polar ibom (DTR 180.0) 1255.97)) (setq bom2 (polar bom2t (DTR 270.0) 673.09)) ;aa2
				(setq bom3t (polar ibom (DTR 180.0) 929.66)) (setq bom3 (polar bom3t (DTR 270.0) 674.35)) ;aa3
				(setq bom4t (polar ibom (DTR 180.0) 570.97)) (setq bom4 (polar bom4t (DTR 270.0) 674.35)) ;aa4
				(setq bom5t (polar ibom (DTR 180.0) 1678.94)) (setq bom5 (polar bom5t (DTR 270.0) 733.15)) ;aa5
				(setq bom6t (polar ibom (DTR 180.0) 1255.97)) (setq bom6 (polar bom6t (DTR 270.0) 733.15)) ;aa6
				(setq bom7t (polar ibom (DTR 180.0) 909.5)) (setq bom7 (polar bom7t (DTR 270.0) 736.51)) ;aa7
				(setq bom8t (polar ibom (DTR 180.0) 550.81)) (setq bom8 (polar bom8t (DTR 270.0) 736.51)) ;aa8
				(setq bom9t (polar ibom (DTR 180.0) 1678.94)) (setq bom9 (polar bom9t (DTR 270.0) 797.41)) ;aa9
				(setq bom10t (polar ibom (DTR 180.0) 1255.97)) (setq bom10 (polar bom10t (DTR 270.0) 797.41)) ;aa10
				(setq bom11t (polar ibom (DTR 180.0) 909.5)) (setq bom11 (polar bom11t (DTR 270.0) 797.41)) ;aa11
				(setq bom12t (polar ibom (DTR 180.0) 550.81)) (setq bom12 (polar bom12t (DTR 270.0) 798.67)) ;aa12
				(setq bom13t (polar ibom (DTR 180.0) 941.84)) (setq bom13 (polar bom13t (DTR 270.0) 860.83)) ;aa13
				(setq bom14t (polar ibom (DTR 180.0) 583.15)) (setq bom14 (polar bom14t (DTR 270.0) 860.83)) ;aa14
				(setq bom15t (polar ibom (DTR 180.0) 953.6)) (setq bom15 (polar bom15t (DTR 270.0) 1403.89)) ;aa15
				(setq bom16t (polar ibom (DTR 180.0) 594.91)) (setq bom16 (polar bom16t (DTR 270.0) 1403.89)) ;aa16
				(setq bom17t (polar ibom (DTR 180.0) 953.6)) (setq bom17 (polar bom17t (DTR 270.0) 1535.78)) ;aa17
				(setq bom18t (polar ibom (DTR 180.0) 594.91)) (setq bom18 (polar bom18t (DTR 270.0) 1535.78)) ;aa18
				(setq bom19t (polar ibom (DTR 180.0) 670.68)) (setq bom19 (polar bom19t (DTR 270.0) 1591.64)) ;aa19
				
				(command "style" "TRB" "Trebuchet MS" (* 2.0 sf) "1" "" "" "")
				(command "TEXT" bom1 "0" newstud1_memname)
				(command "TEXT" bom2 "0" (itoa (Number_Round (+ newstud1_memlen 50) 1)))
				(command "TEXT" bom3 "0" (rtos N3 2 2))
				(command "TEXT" bom4 "0" (rtos (* 1.035 N3 ) 2 2))
				
				(command "TEXT" bom5 "0" newst1_newmem1_memname)
				(command "TEXT" bom6 "0" (itoa (Number_Round (+ newst1_newmem1_len 50) 1)))
				(command "TEXT" bom7 "0" (rtos N4 2 2))
				(command "TEXT" bom8 "0" (rtos (* 1.035 N4 ) 2 2))
				
				(command "TEXT" bom9 "0" (strcat "PL" (rtos clampstk 2 0) "x50"))
				(command "TEXT" bom10 "0" (rtos (Number_Round newst1_newmem1_stx1 1) 2 0))
				(command "TEXT" bom11 "0" (rtos N5 2 2))
				(command "TEXT" bom12 "0" (rtos (* 1.035 N5 ) 2 2))
				
				(command "TEXT" bom19 "0" (rtos concrete_vol 2 3))	
				(command "style" "WMF-Trebuchet MS0" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" bom13 "0" (rtos total_mem_weight 2 2))
				(command "TEXT" bom14 "0" (rtos (* total_mem_weight 1.035) 2 2))
				(command "TEXT" bom15 "0" (rtos total_mem_weight 2 2))
				(command "TEXT" bom16 "0" (rtos (* total_mem_weight 1.035) 2 2))
				(command "TEXT" bom17 "0" (rtos total_weight 2 2))
				(command "TEXT" bom18 "0" (rtos (* total_weight 1.035) 2 2))
				
				
				(setq next1 (polar ibp (dtr 0.0) 9900))
				(setq ibp next1)
		))	
		
		(if (and (= newstudnum 1) (= newstud1_memty "P") (= newst_mem_onpole 1) (= newst_mem_onstruct 1) (= newst1_newmem1_type "P") (= hip_mem 1) (= hip_mem1_type "P") ) (progn
				(if (= ibp nil) (setq ibp '(0.0 0.0 0.0)))
				(if (= polenumber nil) (setq polenumber 1) (setq polenumber (1+ polenumber)))
				(command "_insert" "c:/detail/bolckss/IN-6.0M-RIT-CNTR-MP_P_SOL3" ibp "" "" "" )
				
				(setq dm1t (polar ibp (DTR 0.0) 825.29)) (setq dm1 (polar dm1t (DTR 90.0) 2079.96)) ;b1
				(setq dm2t (polar ibp (DTR 0.0) 772.69)) (setq dm2 (polar dm2t (DTR 90.0) 3476.47)) ;b2
				(setq dm3t (polar ibp (DTR 0.0) 750.06)) (setq dm3 (polar dm3t (DTR 90.0) 4891.83)) ;b3
				(setq dm4t (polar ibp (DTR 0.0) 825.29)) (setq dm4 (polar dm4t (DTR 90.0) 5829.96)) ;b4
				(setq dm5t (polar ibp (DTR 0.0) 2989.11)) (setq dm5 (polar dm5t (DTR 90.0) 2000.29)) ;b5
				(setq dm6t (polar ibp (DTR 0.0) 1795.48)) (setq dm6 (polar dm6t (DTR 90.0) 1508.41)) ;b6
				(setq dm7t (polar ibp (DTR 0.0) 1506.08)) (setq dm7 (polar dm7t (DTR 90.0) 1877.72)) ;b7
				(setq dm8t (polar ibp (DTR 0.0) 2631.08)) (setq dm8 (polar dm8t (DTR 90.0) 1877.72)) ;b8
				(setq dm9t (polar ibp (DTR 0.0) 3169.68)) (setq dm9 (polar dm9t (DTR 90.0) 943.26)) ;b9
				(setq dm10t (polar ibp (DTR 0.0) 3929.66)) (setq dm10 (polar dm10t (DTR 90.0) 942.04)) ;b10
				(setq dm11t (polar ibp (DTR 0.0) 6511.42)) (setq dm11 (polar dm11t (DTR 90.0) 6140)) ;b11
				(setq dm12t (polar ibp (DTR 0.0) 5290.15)) (setq dm12 (polar dm12t (DTR 90.0) 6023.38)) ;b12
				(setq dm13t (polar ibp (DTR 0.0) 5373.55)) (setq dm13 (polar dm13t (DTR 90.0) 6023.38)) ;b13
				(setq dm14t (polar ibp (DTR 0.0) 5461.94)) (setq dm14 (polar dm14t (DTR 90.0) 6023.38)) ;b14
				(setq dm15t (polar ibp (DTR 0.0) 5290.6)) (setq dm15 (polar dm15t (DTR 90.0) 5948.38)) ;b15
				(setq dm16t (polar ibp (DTR 0.0) 5290.15)) (setq dm16 (polar dm16t (DTR 90.0) 5880.29)) ;b16
				(setq dm17t (polar ibp (DTR 0.0) 6511.42)) (setq dm17 (polar dm17t (DTR 90.0) 4555.77)) ;b17
				(setq dm18t (polar ibp (DTR 0.0) 5126.10)) (setq dm18 (polar dm18t (DTR 90.0) 2750.36)) ;b18
				(setq dm19t (polar ibp (DTR 0.0) 4812.18)) (setq dm19 (polar dm19t (DTR 90.0) 2362.58)) ;b19
				(setq dm20t (polar ibp (DTR 0.0) 5201.45)) (setq dm20 (polar dm20t (DTR 90.0) 2203.66)) ;b20
				(setq dm21t (polar ibp (DTR 0.0) 4959.04)) (setq dm21 (polar dm21t (DTR 90.0) 2636.93)) ;b21
				(setq dm22t (polar ibp (DTR 0.0) 5539.77)) (setq dm22 (polar dm22t (DTR 90.0) 2636.93)) ;b22
				(setq dm23t (polar ibp (DTR 0.0) 5969.08)) (setq dm23 (polar dm23t (DTR 90.0) 2636.92)) ;b23
				(setq dm24t (polar ibp (DTR 0.0) 4856.11)) (setq dm24 (polar dm24t (DTR 90.0) 2107.06)) ;b24
				(setq dm25t (polar ibp (DTR 0.0) 5546.79)) (setq dm25 (polar dm25t (DTR 90.0) 2107.06)) ;b25
				(setq dm26t (polar ibp (DTR 0.0) 4812.27)) (setq dm26 (polar dm26t (DTR 90.0) 1795.19)) ;b26
				(setq dm27t (polar ibp (DTR 0.0) 6315.31)) (setq dm27 (polar dm27t (DTR 90.0) 2221.9)) ;b27
				(setq dm28t (polar ibp (DTR 0.0) 6371.83)) (setq dm28 (polar dm28t (DTR 90.0) 2221.9)) ;b28
				(setq dm29t (polar ibp (DTR 0.0) 6035.25)) (setq dm29 (polar dm29t (DTR 90.0) 2021.9)) ;b29
				(setq dm30t (polar ibp (DTR 0.0) 6371.83)) (setq dm30 (polar dm30t (DTR 90.0) 2021.9)) ;b30
				(setq dm31t (polar ibp (DTR 0.0) 5929.95)) (setq dm31 (polar dm31t (DTR 90.0) 1799.97)) ;b31
				(setq dm32t (polar ibp (DTR 0.0) 5201.45)) (setq dm32 (polar dm32t (DTR 90.0) 1573.65)) ;b32
				(setq dm33t (polar ibp (DTR 0.0) 4856.11)) (setq dm33 (polar dm33t (DTR 90.0) 1477.05)) ;b33
				(setq dm34t (polar ibp (DTR 0.0) 5546.79)) (setq dm34 (polar dm34t (DTR 90.0) 1477.05)) ;b34
				(setq dm35t (polar ibp (DTR 0.0) 4812.27)) (setq dm35 (polar dm35t (DTR 90.0) 1165.18)) ;b35
				(setq dm36t (polar ibp (DTR 0.0) 8750)) (setq dm36 (polar dm36t (DTR 90.0) 6140)) ;b36
				(setq dm37t (polar ibp (DTR 0.0) 3378.14)) (setq dm37 (polar dm37t (DTR 90.0) 5055.22)) ;b37
				(setq dm38t (polar ibp (DTR 0.0) 3349.81)) (setq dm38 (polar dm38t (DTR 90.0) 3968)) ;b38
				(setq dm39t (polar ibp (DTR 0.0) 7911.03)) (setq dm39 (polar dm39t (DTR 90.0) 2457.33)) ;b39
				(setq dm40t (polar ibp (DTR 0.0) 8023.51)) (setq dm40 (polar dm40t (DTR 90.0) 2457.33)) ;b40
				(setq dm41t (polar ibp (DTR 0.0) 8158.23)) (setq dm41 (polar dm41t (DTR 90.0) 2457.33)) ;b41
				(setq dm42t (polar ibp (DTR 0.0) 7828.23)) (setq dm42 (polar dm42t (DTR 90.0) 2374.81)) ;b42
				(setq dm43t (polar ibp (DTR 0.0) 7828.23)) (setq dm43 (polar dm43t (DTR 90.0) 2262.33)) ;b43
				(setq dm44t (polar ibp (DTR 0.0) 7828.23)) (setq dm44 (polar dm44t (DTR 90.0) 2112.33)) ;b44
				(setq dm45t (polar ibp (DTR 0.0) 3349.81)) (setq dm45 (polar dm45t (DTR 90.0) 2773)) ;b45

				(command "dim1" "ver" dm1 dm2 (polar dm1 (DTR 180.0) 190) (fix newst1_newmem1_y1))
				(command "dim1" "ver" dm2 dm3 (polar dm1 (DTR 180.0) 190) (fix (- newstud1_y1 newst1_newmem1_y1)))
				(command "dim1" "ver" dm3 dm4 (polar dm1 (DTR 180.0) 190) (fix (- joint_0 newstud1_y1)))
				(command "dim1" "hor" dm7 dm8 (polar dm7 (DTR 270.0) 160) (fix newstud1_x1 ))
				(command "dim1" "hor" dm9 dm10 (polar dm9 (DTR 270.0) 170) (fix newstud1_x1))
				
				(command "style" "TRB" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" dm5 "0" (strcat (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdht 2 0 )))
				(command "TEXT" dm6 "0" (strcat "(POLE-" (itoa polenumber) ")"))
				(command "_insert" "c:/detail/POLEDATA/POLE-L4_120x3" dm11 "" "" "" )				
				(command "_insert" "c:/detail/POLEDATA/POLE-L14_120x3" dm17 "" "" "" )				

				(command "dim1" "hor" dm12 dm13 (polar dm13 (DTR 90.0) 42.5) newstud1_platex2)
				(command "dim1" "hor" dm13 dm14 (polar dm13 (DTR 90.0) 42.5) newstud1_platex1)
				(command "dim1" "ver" dm12 dm15 (polar dm15 (DTR 180.0) 47) newstud1_platey2)
				(command "dim1" "ver" dm15 dm16 (polar dm15 (DTR 180.0) 47) newstud1_platey1)
				
				(if (= newst1_size 48.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_48.3_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_48.3_2BOLTS" dm37 "" "" "" )))
				(if (= newst1_size 60.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_60.3_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_60.3_2BOLTS" dm37 "" "" "" )))
				(if (= newst1_size 76.1) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_76.1_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_76.1_2BOLTS" dm37 "" "" "" )))
				(if (= newst1_size 88.9) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_88.9_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_88.9_2BOLTS" dm37 "" "" "" )))
				(if (= newst1_size 101.6) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_101.6_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_101.6_2BOLTS" dm37 "" "" "" )))
				(if (= newst1_size 114.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_114.3_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_114.3_2BOLTS" dm37 "" "" "" )))
				(if (= newst1_size 139.7) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_2BOLT_139.7_120" dm18 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_139.7_2BOLTS" dm37 "" "" "" )))
	
				(if (= newst1_newmem1_size 48.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_48.3" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_48.3_1BOLTS" dm38 "" "" "" )))
				(if (= newst1_newmem1_size 60.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_60.3" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_60.3_1BOLTS" dm38 "" "" "" )))
				(if (= newst1_newmem1_size 76.1) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_76.1" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_76.1_1BOLTS" dm38 "" "" "" )))
				(if (= newst1_newmem1_size 88.9) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_88.9" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_88.9_1BOLTS" dm38 "" "" "" )))
				(if (= newst1_newmem1_size 101.6) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_101.6" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_101.6_1BOLTS" dm38 "" "" "" )))
				(if (= newst1_newmem1_size 114.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_114.3" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_114.3_1BOLTS" dm38 "" "" "" )))
				(if (= newst1_newmem1_size 139.7) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_139.7" dm20 "" "" "" ) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_139.7_1BOLTS" dm38 "" "" "" )))
				
				(if (= newstud1_hipmem1_size 48.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_48.3" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_48.3_1BOLTS" dm45 "" "" "" ))))
				(if (= newstud1_hipmem1_size 60.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_60.3" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_60.3_1BOLTS" dm45 "" "" "" ))))
				(if (= newstud1_hipmem1_size 76.1) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_76.1" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_76.1_1BOLTS" dm45 "" "" "" ))))
				(if (= newstud1_hipmem1_size 88.9) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_88.9" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_88.9_1BOLTS" dm45 "" "" "" ))))
				(if (= newstud1_hipmem1_size 101.6) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_101.6" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_101.6_1BOLTS" dm45 "" "" "" ))))
				(if (= newstud1_hipmem1_size 114.3) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_114.3" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_114.3_1BOLTS" dm45 "" "" "" ))))
				(if (= newstud1_hipmem1_size 139.7) (progn (command "_insert" "c:/detail/POLEDATA/PIPE_SINGLEBOLT_139.7" dm32 "" "" "" ) (if (/= newstud1_hipmem1_size newst1_newmem1_size) (command "_insert" "c:/detail/POLEDATA/PIPEPRESS_139.7_1BOLTS" dm45 "" "" "" ))))
				
				(command "dim1" "hor" dm21 dm22 (polar dm22 (DTR 270.0) 181) (- newst1_joint2 (+ newst1_sizename2 35) 40))
				(command "dim1" "hor" dm22 dm23 (polar dm22 (DTR 270.0) 181) (- newst1_joint1 (+ newst1_sizename2 35) 40))
				(command "_insert" "f3x" dm19 "0.0394" "0.0394" "" "N3" (strcat newstud1_memname ".." (itoa (Number_Round (+ newstud1_memlen 50) 1)) " Lg"))
				
				(command "dim1" "hor" dm24 dm25 (polar dm24 (DTR 270.0) 181) (Number_Round (- newst1_newmem1_len (+ newst1_newmem1_sizename2 35) (+ newst1_newmem1_sizename2 35)) 1))
				(command "_insert" "f3x" dm26 "0.0394" "0.0394" "" "N4" (strcat newstud1_memname ".." (itoa (Number_Round (+ newst1_newmem1_len 50) 1)) " Lg"))
				
				(command "dim1" "hor" dm27 dm28 (polar dm27 (DTR 90.0) 71) newst1_newmem1_stx2)
				(command "dim1" "hor" dm29 dm30 (polar dm29 (DTR 270.0) 90) (Number_Round newst1_newmem1_stx1 1))
				(command "_insert" "f3x" dm31 "0.0394" "0.0394" "" "N5" (strcat "PLT" (itoa (fix clampstk)) "x50.." (itoa (Number_Round newst1_newmem1_stx1 1)) " Lg"))
				
				(command "dim1" "hor" dm33 dm34 (polar dm34 (DTR 270.0) 181) (- (Number_Round newstud1_hipmem1_len 1) (+ newstud1_hipmem1_sizename2 35) (+ newstud1_hipmem1_sizename2 35)))
				(command "_insert" "f3x" dm35 "0.0394" "0.0394" "" "N6" (strcat newstud1_hipmem1_memname ".." (itoa (Number_Round (+ newstud1_hipmem1_len 50) 1)) " Lg"))
				
				;(command "line" (polar dm19 (DTR 7.0) 119) (polar (polar dm19 (DTR 7.0) 119) (DTR 0.0) 640) "") 
				(command "dim1" "hor" dm39 dm40 (polar dm40 (DTR 90.0) 79) newstud1_platex1)
				(command "dim1" "hor" dm40 dm41 (polar dm40 (DTR 90.0) 79) newstud1_platex2)
				(command "dim1" "ver" dm42 dm43 (polar dm43 (DTR 180.0) 79) newstud1_platey1)
				(command "dim1" "ver" dm43 dm44 (polar dm43 (DTR 180.0) 79) newstud1_platey2)
				
				;BOM
				(setq ibom dm36)
				(setq N3 (* (/ (Number_Round (+ newstud1_memlen 50) 1) 1000.0) newstud1_blockwt 3))
				(setq N4 (* (/ (Number_Round (+ newst1_newmem1_len 50) 1) 1000.0) newst1_newmem1_blockwt 3))
				(setq N5 (* (/ clampstk 1000.0 )(/ 50 1000.0) (/ (Number_Round newst1_newmem1_stx1 1) 1000.0) 7850.0 3))
				(setq N6 (* (/ (Number_Round (+ newstud1_hipmem1_len 50) 1) 1000.0) newst1_newmem1_blockwt 3))
				
				(setq concrete_vol (* (/ newstud1_pdwt 1000.0) (/ newstud1_pdwt 1000.0) (/ newstud1_pdht 1000.0) 3))
				(setq total_mem_weight (+ N3 N4 N5 N6 28.84))
				(setq total_weight (+ total_mem_weight 6.36))
				(command "_insert" "c:/detail/POLEDATA/IN-6.0M-RIT-120-CNTR-MP_P_SOL3_bom" ibom "" "" "" )
				
				(setq bom1t (polar ibom (DTR 180.0) 1678.94)) (setq bom1 (polar bom1t (DTR 270.0) 673.09)) ;aa1
				(setq bom2t (polar ibom (DTR 180.0) 1255.97)) (setq bom2 (polar bom2t (DTR 270.0) 673.09)) ;aa2
				(setq bom3t (polar ibom (DTR 180.0) 929.66)) (setq bom3 (polar bom3t (DTR 270.0) 674.35)) ;aa3
				(setq bom4t (polar ibom (DTR 180.0) 570.97)) (setq bom4 (polar bom4t (DTR 270.0) 674.35)) ;aa4
				(setq bom5t (polar ibom (DTR 180.0) 1678.94)) (setq bom5 (polar bom5t (DTR 270.0) 733.15)) ;aa5
				(setq bom6t (polar ibom (DTR 180.0) 1255.97)) (setq bom6 (polar bom6t (DTR 270.0) 733.15)) ;aa6
				(setq bom7t (polar ibom (DTR 180.0) 909.5)) (setq bom7 (polar bom7t (DTR 270.0) 736.51)) ;aa7
				(setq bom8t (polar ibom (DTR 180.0) 550.81)) (setq bom8 (polar bom8t (DTR 270.0) 736.51)) ;aa8
				(setq bom9t (polar ibom (DTR 180.0) 1678.94)) (setq bom9 (polar bom9t (DTR 270.0) 797.41)) ;aa9
				(setq bom10t (polar ibom (DTR 180.0) 1255.97)) (setq bom10 (polar bom10t (DTR 270.0) 797.41)) ;aa10
				(setq bom11t (polar ibom (DTR 180.0) 909.5)) (setq bom11 (polar bom11t (DTR 270.0) 797.41)) ;aa11
				(setq bom12t (polar ibom (DTR 180.0) 550.81)) (setq bom12 (polar bom12t (DTR 270.0) 798.67)) ;aa12
				(setq bom13t (polar ibom (DTR 180.0) 941.84)) (setq bom13 (polar bom13t (DTR 270.0) 860.83)) ;aa13
				(setq bom14t (polar ibom (DTR 180.0) 583.15)) (setq bom14 (polar bom14t (DTR 270.0) 860.83)) ;aa14
				(setq bom15t (polar ibom (DTR 180.0) 953.6)) (setq bom15 (polar bom15t (DTR 270.0) 1403.89)) ;aa15
				(setq bom16t (polar ibom (DTR 180.0) 594.91)) (setq bom16 (polar bom16t (DTR 270.0) 1403.89)) ;aa16
				(setq bom17t (polar ibom (DTR 180.0) 953.6)) (setq bom17 (polar bom17t (DTR 270.0) 1535.78)) ;aa17
				(setq bom18t (polar ibom (DTR 180.0) 594.91)) (setq bom18 (polar bom18t (DTR 270.0) 1535.78)) ;aa18
				(setq bom19t (polar ibom (DTR 180.0) 670.68)) (setq bom19 (polar bom19t (DTR 270.0) 1591.64)) ;aa19
				
				(command "style" "TRB" "Trebuchet MS" (* 2.0 sf) "1" "" "" "")
				(command "TEXT" bom1 "0" newstud1_memname)
				(command "TEXT" bom2 "0" (itoa (Number_Round (+ newstud1_memlen 50) 1)))
				(command "TEXT" bom3 "0" (rtos N3 2 2))
				(command "TEXT" bom4 "0" (rtos (* 1.035 N3 ) 2 2))
				
				(command "TEXT" bom5 "0" newst1_newmem1_memname)
				(command "TEXT" bom6 "0" (itoa (Number_Round (+ newst1_newmem1_len 50) 1)))
				(command "TEXT" bom7 "0" (rtos N4 2 2))
				(command "TEXT" bom8 "0" (rtos (* 1.035 N4 ) 2 2))
				
				(command "TEXT" bom9 "0" (strcat "PL" (rtos clampstk 2 0) "x50"))
				(command "TEXT" bom10 "0" (rtos (Number_Round newst1_newmem1_stx1 1) 2 0))
				(command "TEXT" bom11 "0" (rtos N5 2 2))
				(command "TEXT" bom12 "0" (rtos (* 1.035 N5 ) 2 2))
				
				(command "TEXT" bom5 "0" newstud1_hipmem1_memname)
				(command "TEXT" bom6 "0" (itoa (Number_Round (+ newstud1_hipmem1_len 50) 1)))
				(command "TEXT" bom7 "0" (rtos N6 2 2))
				(command "TEXT" bom8 "0" (rtos (* 1.035 N6 ) 2 2))
				
				(command "TEXT" bom19 "0" (rtos concrete_vol 2 3))	
				
				(command "style" "WMF-Trebuchet MS0" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" bom13 "0" (rtos total_mem_weight 2 2))
				(command "TEXT" bom14 "0" (rtos (* total_mem_weight 1.035) 2 2))
				(command "TEXT" bom15 "0" (rtos total_mem_weight 2 2))
				(command "TEXT" bom16 "0" (rtos (* total_mem_weight 1.035) 2 2))
				(command "TEXT" bom17 "0" (rtos total_weight 2 2))
				(command "TEXT" bom18 "0" (rtos (* total_weight 1.035) 2 2))
				
				
				(setq next1 (polar ibp (dtr 0.0) 9900))
				(setq ibp next1)
				
		))
		
		(if (and (= newstudnum 1) (= newstud1_memty "P") (= newst_mem_onpole 2) (= newst_mem_onstruct 2) (= newst1_newmem1_type "P") (= hip_mem 0) ) (progn
				(if (= ibp nil) (setq ibp '(0.0 0.0 0.0)))
				(if (= polenumber nil) (setq polenumber 1) (setq polenumber (1+ polenumber)))
				(command "_insert" "c:/detail/bolckss/IN-6.0M-RIT-CNTR-MP_P_SOL4" ibp "" "" "" )
				
				(setq dm1t (polar ibp (DTR 0.0) 1019.55))  (setq dm1 (polar dm1t (DTR 90.0) 2079.97))  ;J1
				(setq dm2t (polar ibp (DTR 0.0) 1381.08))  (setq dm2 (polar dm2t (DTR 90.0) 2329.97))  ;J2
				(setq dm3t (polar ibp (DTR 0.0) 882.31))  (setq dm3 (polar dm3t (DTR 90.0) 2079.97))  ;J3
				(setq dm4t (polar ibp (DTR 0.0) 1426.08))  (setq dm4 (polar dm4t (DTR 90.0) 3953.72))  ;J4
				(setq dm5t (polar ibp (DTR 0.0) 1443.58))  (setq dm5 (polar dm5t (DTR 90.0) 5829.97))  ;J5
				(setq dm6t (polar ibp (DTR 0.0) 825.3))  (setq dm6 (polar dm6t (DTR 90.0) 2079.97))  ;J6
				(setq dm7t (polar ibp (DTR 0.0) 772.69))  (setq dm7 (polar dm7t (DTR 90.0) 2936.52))  ;J7
				(setq dm8t (polar ibp (DTR 0.0) 772.69))  (setq dm8 (polar dm8t (DTR 90.0) 3903.65))  ;J8
				(setq dm9t (polar ibp (DTR 0.0) 750.06))  (setq dm9 (polar dm9t (DTR 90.0) 4891.84))  ;J9
				(setq dm10t (polar ibp (DTR 0.0) 825.3))  (setq dm10 (polar dm10t (DTR 90.0) 5829.97))  ;J10
				(setq dm11t (polar ibp (DTR 0.0) 635.03))  (setq dm11 (polar dm11t (DTR 90.0) 2079.97))  ;J11
				(setq dm12t (polar ibp (DTR 0.0) 635.03))  (setq dm12 (polar dm12t (DTR 90.0) 5829.97))  ;J12
				(setq dm13t (polar ibp (DTR 0.0) 1506.08))  (setq dm13 (polar dm13t (DTR 90.0) 1877.72))  ;J13
				(setq dm14t (polar ibp (DTR 0.0) 2443.62))  (setq dm14 (polar dm14t (DTR 90.0) 1877.72))  ;J14
				(setq dm15t (polar ibp (DTR 0.0) 2631.08))  (setq dm15 (polar dm15t (DTR 90.0) 1877.72))  ;J15
				(setq dm16t (polar ibp (DTR 0.0) 1056.24))  (setq dm16 (polar dm16t (DTR 90.0) 3215.91))  ;J16
				(setq dm17t (polar ibp (DTR 0.0) 1056.24))  (setq dm17 (polar dm17t (DTR 90.0) 5216.66))  ;J17
				(setq dm18t (polar ibp (DTR 0.0) 1882.37))  (setq dm18 (polar dm18t (DTR 90.0) 2538.65))  ;J18
				(setq dm19t (polar ibp (DTR 0.0) 2989.11))  (setq dm19 (polar dm19t (DTR 90.0) 2000.29))  ;J19
				(setq dm20t (polar ibp (DTR 0.0) 1795.48))  (setq dm20 (polar dm20t (DTR 90.0) 1508.42))  ;J20
				(setq dm21t (polar ibp (DTR 0.0) 1491.94))  (setq dm21 (polar dm21t (DTR 90.0) 925.05))  ;J21
				(setq dm22t (polar ibp (DTR 0.0) 2166.94))  (setq dm22 (polar dm22t (DTR 90.0) 923.55))  ;J22
				(setq dm23t (polar ibp (DTR 0.0) 3319.4))  (setq dm23 (polar dm23t (DTR 90.0) 943.27))  ;J23
				(setq dm24t (polar ibp (DTR 0.0) 4064.06))  (setq dm24 (polar dm24t (DTR 90.0) 942.05))  ;J24
				(setq dm25t (polar ibp (DTR 0.0) 3395.95))  (setq dm25 (polar dm25t (DTR 90.0) 2685.7))  ;J25
				(setq dm26t (polar ibp (DTR 0.0) 3392.17))  (setq dm26 (polar dm26t (DTR 90.0) 2610.05))  ;J26
				(setq dm27t (polar ibp (DTR 0.0) 4578.22))  (setq dm27 (polar dm27t (DTR 90.0) 3625.58))  ;J27
				(setq dm28t (polar ibp (DTR 0.0) 4851.35))  (setq dm28 (polar dm28t (DTR 90.0) 3625.58))  ;J28
				(setq dm29t (polar ibp (DTR 0.0) 4462.03))  (setq dm29 (polar dm29t (DTR 90.0) 3421.75))  ;J29
				(setq dm30t (polar ibp (DTR 0.0) 4967.54))  (setq dm30 (polar dm30t (DTR 90.0) 3421.75))  ;J30
				(setq dm31t (polar ibp (DTR 0.0) 4435.87))  (setq dm31 (polar dm31t (DTR 90.0) 3223.05))  ;J31
				(setq dm32t (polar ibp (DTR 0.0) 5320.7))  (setq dm32 (polar dm32t (DTR 90.0) 3610.08))  ;J32
				(setq dm33t (polar ibp (DTR 0.0) 5598.28))  (setq dm33 (polar dm33t (DTR 90.0) 3610.08))  ;J33
				(setq dm34t (polar ibp (DTR 0.0) 5202.63))  (setq dm34 (polar dm34t (DTR 90.0) 3402.93))  ;J34
				(setq dm35t (polar ibp (DTR 0.0) 5716.36))  (setq dm35 (polar dm35t (DTR 90.0) 3402.93))  ;J35
				(setq dm36t (polar ibp (DTR 0.0) 5154.2))  (setq dm36 (polar dm36t (DTR 90.0) 3217.27))  ;J36
				(setq dm37t (polar ibp (DTR 0.0) 6106.35))  (setq dm37 (polar dm37t (DTR 90.0) 3672.17))  ;J37
				(setq dm38t (polar ibp (DTR 0.0) 6189.75))  (setq dm38 (polar dm38t (DTR 90.0) 3672.17))  ;J38
				(setq dm39t (polar ibp (DTR 0.0) 6278.14))  (setq dm39 (polar dm39t (DTR 90.0) 3672.17))  ;J39
				(setq dm40t (polar ibp (DTR 0.0) 6106.8))  (setq dm40 (polar dm40t (DTR 90.0) 3597.17))  ;J40
				(setq dm41t (polar ibp (DTR 0.0) 6106.35))  (setq dm41 (polar dm41t (DTR 90.0) 3529.08))  ;J41
				(setq dm42t (polar ibp (DTR 0.0) 5860.49))  (setq dm42 (polar dm42t (DTR 90.0) 3268.45))  ;J42
				(setq dm43t (polar ibp (DTR 0.0) 6043.34))  (setq dm43 (polar dm43t (DTR 90.0) 4015.1))  ;J43
				(setq dm44t (polar ibp (DTR 0.0) 6126.99))  (setq dm44 (polar dm44t (DTR 90.0) 4040.53))  ;J44
				(setq dm45t (polar ibp (DTR 0.0) 6365.59))  (setq dm45 (polar dm45t (DTR 90.0) 4017.44))  ;J45
				(setq dm46t (polar ibp (DTR 0.0) 6365.59))  (setq dm46 (polar dm46t (DTR 90.0) 4032.83))  ;J46
				(setq dm47t (polar ibp (DTR 0.0) 6043))  (setq dm47 (polar dm47t (DTR 90.0) 4315.93))  ;J47
				(setq dm48t (polar ibp (DTR 0.0) 6126.99))  (setq dm48 (polar dm48t (DTR 90.0) 4340.35))  ;J48
				(setq dm49t (polar ibp (DTR 0.0) 6365.59))  (setq dm49 (polar dm49t (DTR 90.0) 4317.26))  ;J49
				(setq dm50t (polar ibp (DTR 0.0) 6365.59))  (setq dm50 (polar dm50t (DTR 90.0) 4332.65))  ;J50
				(setq dm51t (polar ibp (DTR 0.0) 4607.42))  (setq dm51 (polar dm51t (DTR 90.0) 2284.74))  ;J51
				(setq dm52t (polar ibp (DTR 0.0) 4880.56))  (setq dm52 (polar dm52t (DTR 90.0) 2284.74))  ;J52
				(setq dm53t (polar ibp (DTR 0.0) 4491.24))  (setq dm53 (polar dm53t (DTR 90.0) 2080.91))  ;J53
				(setq dm54t (polar ibp (DTR 0.0) 4996.74))  (setq dm54 (polar dm54t (DTR 90.0) 2080.91))  ;J54
				(setq dm55t (polar ibp (DTR 0.0) 4449.63))  (setq dm55 (polar dm55t (DTR 90.0) 1882.21))  ;J55
				(setq dm56t (polar ibp (DTR 0.0) 5349.91))  (setq dm56 (polar dm56t (DTR 90.0) 2269.24))  ;J56
				(setq dm57t (polar ibp (DTR 0.0) 5627.49))  (setq dm57 (polar dm57t (DTR 90.0) 2269.24))  ;J57
				(setq dm58t (polar ibp (DTR 0.0) 5231.83))  (setq dm58 (polar dm58t (DTR 90.0) 2062.1))  ;J58
				(setq dm59t (polar ibp (DTR 0.0) 5745.56))  (setq dm59 (polar dm59t (DTR 90.0) 2062.1))  ;J59
				(setq dm60t (polar ibp (DTR 0.0) 5183.41))  (setq dm60 (polar dm60t (DTR 90.0) 1876.44))  ;J60
				(setq dm61t (polar ibp (DTR 0.0) 5875.03))  (setq dm61 (polar dm61t (DTR 90.0) 1927.74))  ;J61
				(setq dm62t (polar ibp (DTR 0.0) 6053.15))  (setq dm62 (polar dm62t (DTR 90.0) 2673.63))  ;J62
				(setq dm63t (polar ibp (DTR 0.0) 6136.99))  (setq dm63 (polar dm63t (DTR 90.0) 2698.45))  ;J63
				(setq dm64t (polar ibp (DTR 0.0) 6375.59))  (setq dm64 (polar dm64t (DTR 90.0) 2675.37))  ;J64
				(setq dm65t (polar ibp (DTR 0.0) 6375.59))  (setq dm65 (polar dm65t (DTR 90.0) 2690.76))  ;J65
				(setq dm66t (polar ibp (DTR 0.0) 6052.42))  (setq dm66 (polar dm66t (DTR 90.0) 2975.37))  ;J66
				(setq dm67t (polar ibp (DTR 0.0) 6136.99))  (setq dm67 (polar dm67t (DTR 90.0) 2998.27))  ;J67
				(setq dm68t (polar ibp (DTR 0.0) 6375.59))  (setq dm68 (polar dm68t (DTR 90.0) 2975.19))  ;J68
				(setq dm69t (polar ibp (DTR 0.0) 6375.59))  (setq dm69 (polar dm69t (DTR 90.0) 2990.58))  ;J69
				(setq dm70t (polar ibp (DTR 0.0) 6803.82))  (setq dm70 (polar dm70t (DTR 90.0) 5792.7))  ;J70
				(setq dm71t (polar ibp (DTR 0.0) 6803.82))  (setq dm71 (polar dm71t (DTR 90.0) 5872.69))  ;J71
				(setq dm72t (polar ibp (DTR 0.0) 6803.82))  (setq dm72 (polar dm72t (DTR 90.0) 5952.7))  ;J72
				(setq dm73t (polar ibp (DTR 0.0) 7032.86))  (setq dm73 (polar dm73t (DTR 90.0) 5952.7))  ;J73
				(setq dm74t (polar ibp (DTR 0.0) 7212.86))  (setq dm74 (polar dm74t (DTR 90.0) 5933))  ;J74
				(setq dm75t (polar ibp (DTR 0.0) 8223.68))  (setq dm75 (polar dm75t (DTR 90.0) 5932.95))  ;J75
				(setq dm76t (polar ibp (DTR 0.0) 8403.68))  (setq dm76 (polar dm76t (DTR 90.0) 5952.65))  ;J76
				(setq dm77t (polar ibp (DTR 0.0) 8463.68))  (setq dm77 (polar dm77t (DTR 90.0) 5792.65))  ;J77
				(setq dm78t (polar ibp (DTR 0.0) 8223.68))  (setq dm78 (polar dm78t (DTR 90.0) 5812.35))  ;J78
				(setq dm79t (polar ibp (DTR 0.0) 7972.68))  (setq dm79 (polar dm79t (DTR 90.0) 5812.4))  ;J79
				(setq dm80t (polar ibp (DTR 0.0) 7517.68))  (setq dm80 (polar dm80t (DTR 90.0) 5812.4))  ;J80
				(setq dm81t (polar ibp (DTR 0.0) 7212.86))  (setq dm81 (polar dm81t (DTR 90.0) 5812.4))  ;J81
				(setq dm82t (polar ibp (DTR 0.0) 6972.08))  (setq dm82 (polar dm82t (DTR 90.0) 5792.65))  ;J82
				(setq dm83t (polar ibp (DTR 0.0) 7721.28))  (setq dm83 (polar dm83t (DTR 90.0) 5812.4))  ;J83
				(setq dm84t (polar ibp (DTR 0.0) 7731.19))  (setq dm84 (polar dm84t (DTR 90.0) 5933))  ;J84
				(setq dm85t (polar ibp (DTR 0.0) 7318.64))  (setq dm85 (polar dm85t (DTR 90.0) 5528.14))  ;J85
				(setq dm86t (polar ibp (DTR 0.0) 6808.52))  (setq dm86 (polar dm86t (DTR 90.0) 5207.58))  ;J86
				(setq dm87t (polar ibp (DTR 0.0) 6808.52))  (setq dm87 (polar dm87t (DTR 90.0) 5276.58))  ;J87
				(setq dm88t (polar ibp (DTR 0.0) 6808.52))  (setq dm88 (polar dm88t (DTR 90.0) 5345.58))  ;J88
				(setq dm89t (polar ibp (DTR 0.0) 6938.52))  (setq dm89 (polar dm89t (DTR 90.0) 5345.58))  ;J89
				(setq dm90t (polar ibp (DTR 0.0) 7088.52))  (setq dm90 (polar dm90t (DTR 90.0) 5324.88))  ;J90
				(setq dm91t (polar ibp (DTR 0.0) 7779.2))  (setq dm91 (polar dm91t (DTR 90.0) 5324.88))  ;J91
				(setq dm92t (polar ibp (DTR 0.0) 7929.2))  (setq dm92 (polar dm92t (DTR 90.0) 5345.58))  ;J92
				(setq dm93t (polar ibp (DTR 0.0) 8009.2))  (setq dm93 (polar dm93t (DTR 90.0) 5207.58))  ;J93
				(setq dm94t (polar ibp (DTR 0.0) 7779.2))  (setq dm94 (polar dm94t (DTR 90.0) 5228.28))  ;J94
				(setq dm95t (polar ibp (DTR 0.0) 7088.52))  (setq dm95 (polar dm95t (DTR 90.0) 5228.28))  ;J95
				(setq dm96t (polar ibp (DTR 0.0) 6858.52))  (setq dm96 (polar dm96t (DTR 90.0) 5207.58))  ;J96
				(setq dm97t (polar ibp (DTR 0.0) 7361.5))  (setq dm97 (polar dm97t (DTR 90.0) 5228.28))  ;J97
				(setq dm98t (polar ibp (DTR 0.0) 7361.5))  (setq dm98 (polar dm98t (DTR 90.0) 5324.88))  ;J98
				(setq dm99t (polar ibp (DTR 0.0) 7021.8))  (setq dm99 (polar dm99t (DTR 90.0) 4912.87))  ;J99
				(setq dm100t (polar ibp (DTR 0.0) 5233.88))  (setq dm100 (polar dm100t (DTR 90.0) 1528.22))  ;J100
				(setq dm101t (polar ibp (DTR 0.0) 5233.88))  (setq dm101 (polar dm101t (DTR 90.0) 1597.22))  ;J101
				(setq dm102t (polar ibp (DTR 0.0) 5233.88))  (setq dm102 (polar dm102t (DTR 90.0) 1666.22))  ;J102
				(setq dm103t (polar ibp (DTR 0.0) 5363.88))  (setq dm103 (polar dm103t (DTR 90.0) 1666.22))  ;J103
				(setq dm104t (polar ibp (DTR 0.0) 5513.88))  (setq dm104 (polar dm104t (DTR 90.0) 1645.52))  ;J104
				(setq dm105t (polar ibp (DTR 0.0) 6204.56))  (setq dm105 (polar dm105t (DTR 90.0) 1645.52))  ;J105
				(setq dm106t (polar ibp (DTR 0.0) 6354.56))  (setq dm106 (polar dm106t (DTR 90.0) 1666.22))  ;J106
				(setq dm107t (polar ibp (DTR 0.0) 6434.56))  (setq dm107 (polar dm107t (DTR 90.0) 1528.22))  ;J107
				(setq dm108t (polar ibp (DTR 0.0) 6204.56))  (setq dm108 (polar dm108t (DTR 90.0) 1548.92))  ;J108
				(setq dm109t (polar ibp (DTR 0.0) 5513.88))  (setq dm109 (polar dm109t (DTR 90.0) 1548.92))  ;J109
				(setq dm110t (polar ibp (DTR 0.0) 5283.88))  (setq dm110 (polar dm110t (DTR 90.0) 1528.22))  ;J110
				(setq dm111t (polar ibp (DTR 0.0) 5786.85))  (setq dm111 (polar dm111t (DTR 90.0) 1548.92))  ;J111
				(setq dm112t (polar ibp (DTR 0.0) 5786.85))  (setq dm112 (polar dm112t (DTR 90.0) 1645.52))  ;J112
				(setq dm113t (polar ibp (DTR 0.0) 5470.05))  (setq dm113 (polar dm113t (DTR 90.0) 1237.05))  ;J113
				(setq dm114t (polar ibp (DTR 0.0) 8547.73))  (setq dm114 (polar dm114t (DTR 90.0) 5343.13))  ;J114
				(setq dm115t (polar ibp (DTR 0.0) 8604.25))  (setq dm115 (polar dm115t (DTR 90.0) 5343.13))  ;J115
				(setq dm116t (polar ibp (DTR 0.0) 8267.67))  (setq dm116 (polar dm116t (DTR 90.0) 5143.13))  ;J116
				(setq dm117t (polar ibp (DTR 0.0) 8604.25))  (setq dm117 (polar dm117t (DTR 90.0) 5143.13))  ;J117
				(setq dm118t (polar ibp (DTR 0.0) 8139.48))  (setq dm118 (polar dm118t (DTR 90.0) 4917.65))  ;J118
				(setq dm119t (polar ibp (DTR 0.0) 8062.48))  (setq dm119 (polar dm119t (DTR 90.0) 4270.67))  ;J119
				(setq dm120t (polar ibp (DTR 0.0) 8287.48))  (setq dm120 (polar dm120t (DTR 90.0) 4301.72))  ;J120
				(setq dm121t (polar ibp (DTR 0.0) 8482.48))  (setq dm121 (polar dm121t (DTR 90.0) 4301.72))  ;J121
				(setq dm122t (polar ibp (DTR 0.0) 8482.48))  (setq dm122 (polar dm122t (DTR 90.0) 4198.22))  ;J122
				(setq dm123t (polar ibp (DTR 0.0) 8482.48))  (setq dm123 (polar dm123t (DTR 90.0) 4094.72))  ;J123
				(setq dm124t (polar ibp (DTR 0.0) 8407.48))  (setq dm124 (polar dm124t (DTR 90.0) 4094.72))  ;J124
				(setq dm125t (polar ibp (DTR 0.0) 8062.48))  (setq dm125 (polar dm125t (DTR 90.0) 4125.77))  ;J125
				(setq dm126t (polar ibp (DTR 0.0) 7923.83))  (setq dm126 (polar dm126t (DTR 90.0) 4270.67))  ;J126
				(setq dm127t (polar ibp (DTR 0.0) 7923.83))  (setq dm127 (polar dm127t (DTR 90.0) 4125.77))  ;J127
				(setq dm128t (polar ibp (DTR 0.0) 8071.34))  (setq dm128 (polar dm128t (DTR 90.0) 3168.21))  ;J128
				(setq dm129t (polar ibp (DTR 0.0) 8296.34))  (setq dm129 (polar dm129t (DTR 90.0) 3199.26))  ;J129
				(setq dm130t (polar ibp (DTR 0.0) 8563.33))  (setq dm130 (polar dm130t (DTR 90.0) 3199.26))  ;J130
				(setq dm131t (polar ibp (DTR 0.0) 8563.33))  (setq dm131 (polar dm131t (DTR 90.0) 3095.76))  ;J131
				(setq dm132t (polar ibp (DTR 0.0) 8563.33))  (setq dm132 (polar dm132t (DTR 90.0) 2992.26))  ;J132
				(setq dm133t (polar ibp (DTR 0.0) 8380.74))  (setq dm133 (polar dm133t (DTR 90.0) 2992.26))  ;J133
				(setq dm134t (polar ibp (DTR 0.0) 8071.34))  (setq dm134 (polar dm134t (DTR 90.0) 3023.31))  ;J134
				(setq dm135t (polar ibp (DTR 0.0) 7918.12))  (setq dm135 (polar dm135t (DTR 90.0) 3168.21))  ;J135
				(setq dm136t (polar ibp (DTR 0.0) 7918.12))  (setq dm136 (polar dm136t (DTR 90.0) 3023.31))  ;J136
				(setq dm137t (polar ibp (DTR 0.0) 6837.18))  (setq dm137 (polar dm137t (DTR 90.0) 1720.38))  ;J137
				(setq dm138t (polar ibp (DTR 0.0) 7909.44))  (setq dm138 (polar dm138t (DTR 90.0) 2354.54))  ;J138
				(setq dm139t (polar ibp (DTR 0.0) 8021.92))  (setq dm139 (polar dm139t (DTR 90.0) 2354.54))  ;J139
				(setq dm140t (polar ibp (DTR 0.0) 8156.64))  (setq dm140 (polar dm140t (DTR 90.0) 2354.25))  ;J140
				(setq dm141t (polar ibp (DTR 0.0) 7826.64))  (setq dm141 (polar dm141t (DTR 90.0) 2271.74))  ;J141
				(setq dm142t (polar ibp (DTR 0.0) 7826.64))  (setq dm142 (polar dm142t (DTR 90.0) 2159.25))  ;J142
				(setq dm143t (polar ibp (DTR 0.0) 7826.64))  (setq dm143 (polar dm143t (DTR 90.0) 2009.25))  ;J143
				(setq dm144t (polar ibp (DTR 0.0) 7960.99))  (setq dm144 (polar dm144t (DTR 90.0) 1746.32))  ;J144
				(setq dm145t (polar ibp (DTR 0.0) 6571.84))  (setq dm145 (polar dm145t (DTR 90.0) 6150))  ;J145
				(setq dm146t (polar ibp (DTR 0.0) 1716.95))  (setq dm146 (polar dm146t (DTR 90.0) 2993.53))  ;J146
				(setq dm147t (polar ibp (DTR 0.0) 1662.82))  (setq dm147 (polar dm147t (DTR 90.0) 3780.41))  ;J147
				
				
				(command "dim1" "ver" dm1 dm2 dm3 (fix expole_pedestalhight))
				(command "dim1" "ver" dm3 dm4 dm6 (fix joint_1))
				(command "dim1" "ver" dm4 dm5 dm6 (fix (- joint_0 joint_1)))
				(if(< newst1_newmem1_y1 newst1_newmem2_y1) 
					(progn 
						(command "dim1" "ver" dm6 dm7 dm11 (fix newst1_newmem1_y1))
						(command "dim1" "ver" dm7 dm8 dm11 (- newst1_newmem2_y1 newst1_newmem1_y1))
						(command "dim1" "ver" dm8 dm9 dm11 (fix (- newstud1_y1 newst1_newmem2_y1)))
						(command "dim1" "ver" dm9 dm10 dm11 (fix (- joint_0 newstud1_y1)))
						(command "_insert" "f2" dm146 "0.0394" "0.0394" "" "N4")
						(command "_insert" "f2" dm147 "0.0394" "0.0394" "" "N5")
					)
				)
				(if(< newst1_newmem2_y1 newst1_newmem1_y1) 
					(progn 
						(command "dim1" "ver" dm6 dm7 dm11 (fix newst1_newmem2_y1))
						(command "dim1" "ver" dm7 dm8 dm11 (- newst1_newmem1_y1 newst2_newmem1_y1))
						(command "dim1" "ver" dm8 dm9 dm11 (fix (- newstud1_y1 newst1_newmem1_y1)))
						(command "dim1" "ver" dm9 dm10 dm11 (fix (- joint_0 newstud1_y1)))
						(command "_insert" "f2" dm147 "0.0394" "0.0394" "" "N4")
						(command "_insert" "f2" dm146 "0.0394" "0.0394" "" "N5")
					)
				)
								
				(command "dim1" "ver" dm11 dm12 (polar dm12 (DTR 180.0) 130) (fix joint_0))
				(command "dim1" "hor" dm13 dm14 (polar dm14 (DTR 270.0) 140) (fix existingstud_x))
				(command "dim1" "hor" dm14 dm15 (polar dm14 (DTR 270.0) 140) (fix (- newstud1_x1 existingstud_x)))
				
				(command "style" "TRB" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" dm16 "0" (strcat "%%c" (rtos diameter_1 2 1 ) "x" (rtos diameter_1tick 2 1 ) "Thk"))
				(command "TEXT" dm17 "0" (strcat "%%c" (rtos diameter_2 2 1 ) "x" (rtos diameter_2tick 2 1 ) "Thk"))
				(command "TEXT" dm18 "0" exst1_name)
				(command "TEXT" dm19 "0" (strcat (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdht 2 0 ) ))
				(command "TEXT" dm20 "0" (strcat "(POLE-" (itoa polenumber) ")"))
				(command "dim1" "hor" dm21 dm22 (polar dm22 (DTR 270.0) 167) (fix existingstud_x))
				(command "dim1" "hor" dm23 dm24 (polar dm23 (DTR 270.0) 170) (fix newstud1_x1))
				(command "dim1" "hor" dm27 dm28 (polar dm27 (DTR 90.0) 80.5) (fix newstud1_clampx1))
				(command "dim1" "hor" dm29 dm30 (polar dm30 (DTR 270.0) 81) (fix newstud1_clmapx2))
				(command "_insert" "f5x" dm31 "0.0394" "0.0394" "" "L11-1" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newstud1_clmapx2 2 0)))
				(command "dim1" "hor" dm32 dm33 (polar dm32 (DTR 90.0) 101) (fix newstud1_clampx1))
				(command "dim1" "hor" dm34 dm35 (polar dm35 (DTR 270.0) 78) (fix newstud1_clmapx2))
				(command "_insert" "f5x" dm36 "0.0394" "0.0394" "" "L11-2" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newstud1_clmapx2 2 0)))
				(command "dim1" "hor" dm37 dm38 (polar dm37 (DTR 90.0) 57) newstud1_platex2)
				(command "dim1" "hor" dm38 dm39 (polar dm39 (DTR 90.0) 57) newstud1_platex1)
				(command "dim1" "ver" dm37 dm40 (polar dm40 (DTR 180.0) 89) newstud1_platey2)
				(command "dim1" "ver" dm40 dm41 (polar dm40 (DTR 180.0) 89) newstud1_platey1)
				(command "_insert" "f5x" dm42 "0.0394" "0.0394" "" "L11-3" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x100x150" ))
				(command "dim1" "ali" dm43 dm44 "@" R_cc2 )
				(command "dim1" "ver" dm45 dm46 (polar dm45 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "ali" dm47 dm48 "@" R_cc2 )
				(command "dim1" "ver" dm49 dm50 (polar dm50 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "hor" dm51 dm52 (polar dm51 (DTR 90.0) 80.5) (fix newst1_newmem1_pclampx1))
				(command "dim1" "hor" dm53 dm54 (polar dm53 (DTR 270.0) 81) (fix newst1_newmem1_pclampx2))
				(command "_insert" "f5x" dm55 "0.0394" "0.0394" "" "L12-1" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newst1_newmem1_pclampx2 2 0)))
				(command "dim1" "hor" dm56 dm57 (polar dm56 (DTR 90.0) 101) (fix newst1_newmem1_pclampx1))
				(command "dim1" "hor" dm58 dm59 (polar dm58 (DTR 270.0) 78) (fix newst1_newmem1_pclampx2))
				(command "_insert" "f5x" dm60 "0.0394" "0.0394" "" "L12-2" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newst1_newmem1_pclampx2 2 0)))
				(command "_insert" "f5x" dm61 "0.0394" "0.0394" "" "L12-3" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x100x100" ))
				(command "dim1" "ali" dm62 dm63 "@" R_cc1 )
				(command "dim1" "ver" dm64 dm65 (polar dm64 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "ali" dm66 dm67 "@" R_cc1 )
				(command "dim1" "ver" dm68 dm69 (polar dm68 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "ver" dm71 dm70 (polar dm71 (DTR 180.0) 80) (rtos newstud1_backmark 2 1))
				(command "dim1" "ver" dm71 dm72 (polar dm71 (DTR 180.0) 80) (rtos newstud1_backmark 2 1))
				(command "dim1" "hor" dm73 dm74 (polar dm73 (DTR 90.0) 65) newst1_sizename2)
				(command "dim1" "hor" dm75 dm76 (polar dm75 (DTR 90.0) 85) newst1_sizename2)
				(command "dim1" "hor" dm77 dm78 (polar dm78 (DTR 270.0) 157) (+ newst1_sizename2 35))
				(command "dim1" "hor" dm78 dm79 (polar dm79 (DTR 270.0) 157) (- newst1_joint3 (+ newst1_sizename2 35) 40))
				(command "dim1" "hor" dm79 dm80 (polar dm79 (DTR 270.0) 157) newst1_joint2)
				(command "dim1" "hor" dm80 dm81 (polar dm80 (DTR 270.0) 157) (- newst1_joint1 (+ newst1_sizename2 35) 40))
				(command "dim1" "hor" dm81 dm82 (polar dm81 (DTR 270.0) 157) (+ newst1_sizename2 35))
				(command "dim1" "ver" dm83 dm84 "@" newst1_sizename1)
				(command "_insert" "f3x" dm85 "0.0394" "0.0394" "" "N3" (strcat newstud1_memname ".." (itoa (Number_Round (+ newstud1_memlen 50) 1)) " Lg"))
				
				(command "dim1" "ver" dm87 dm86 (polar dm87 (DTR 180.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "ver" dm87 dm88 (polar dm87 (DTR 180.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "hor" dm89 dm90 (polar dm89 (DTR 90.0) 61) newst1_newmem1_sizename2)
				(command "dim1" "hor" dm91 dm92 (polar dm92 (DTR 90.0) 61) newst1_newmem1_sizename2)
				(command "dim1" "hor" dm93 dm94 (polar dm94 (DTR 270.0) 181) (+ newst1_newmem1_sizename2 35))
				(command "dim1" "hor" dm94 dm95 (polar dm95 (DTR 270.0) 181) (Number_Round (- newst1_newmem1_len (+ newst1_newmem1_sizename2 35) (+ newst1_newmem1_sizename2 35)) 1))
				(command "dim1" "hor" dm95 dm96 (polar dm95 (DTR 270.0) 181) (+ newst1_newmem1_sizename2 35))
				(command "dim1" "ver" dm97 dm98 "@" newst1_newmem1_sizename1)
				(command "_insert" "f3x" dm99 "0.0394" "0.0394" "" "N4" (strcat newst1_newmem1_memname ".." (itoa (Number_Round (+ newst1_newmem1_len 50) 1)) " Lg"))
				
				(command "dim1" "ver" dm101 dm100 (polar dm101 (DTR 180.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "ver" dm101 dm102 (polar dm101 (DTR 180.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "hor" dm103 dm104 (polar dm103 (DTR 90.0) 61) newst1_newmem2_sizename2)
				(command "dim1" "hor" dm105 dm106 (polar dm106 (DTR 90.0) 61) newst1_newmem2_sizename2)
				(command "dim1" "hor" dm107 dm108 (polar dm108 (DTR 270.0) 181) (+ newst1_newmem2_sizename2 35))
				(command "dim1" "hor" dm108 dm109 (polar dm109 (DTR 270.0) 181) (Number_Round (- newst1_newmem2_len (+ newst1_newmem2_sizename2 35) (+ newst1_newmem2_sizename2 35)) 1))
				(command "dim1" "hor" dm109 dm110 (polar dm109 (DTR 270.0) 181) (+ newst1_newmem2_sizename2 35))
				(command "dim1" "ver" dm111 dm112 "@" newst1_newmem2_sizename1)
				(command "_insert" "f3x" dm113 "0.0394" "0.0394" "" "N6" (strcat newst1_newmem2_memname ".." (itoa (Number_Round (+ newst1_newmem2_len 50) 1)) " Lg"))
				
				(command "dim1" "hor" dm114 dm115 (polar dm114 (DTR 90.0) 71) newst1_newmem1_stx2)
				(command "dim1" "hor" dm116 dm117 (polar dm116 (DTR 270.0) 90) (Number_Round newst1_newmem1_stx1 1))
				(command "_insert" "f3x" dm118 "0.0394" "0.0394" "" "N5" (strcat "PLT" (itoa (fix clampstk)) "x50.." (itoa (Number_Round newst1_newmem1_stx1 1)) " Lg"))
				
				(command "dim1" "hor" dm119 dm120 (polar dm120 (DTR 90.0) 61) newst1_newmem2_sizename2)				
				(command "dim1" "ver" dm122 dm121 (polar dm122 (DTR 0.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "ver" dm122 dm123 (polar dm122 (DTR 0.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "hor" dm124 dm125 (polar dm125 (DTR 270.0) 115) (+ newst1_newmem2_sizename2 35))
				(command "dim1" "ver" dm126 dm127 "@" newst1_newmem2_sizename1)
				
				(command "dim1" "hor" dm128 dm129 (polar dm129 (DTR 90.0) 92) newst1_newmem1_sizename2)
				(command "dim1" "ver" dm131 dm130 (polar dm131 (DTR 0.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "ver" dm131 dm132 (polar dm131 (DTR 0.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "hor" dm133 dm134 (polar dm133 (DTR 270.0) 64) (+ newst1_newmem1_sizename2 35))
				(command "dim1" "ver" dm135 dm136 "@" newst1_newmem1_sizename1)
				
				(command "_insert" "f3x" dm137 "0.0394" "0.0394" "" "N1" (strcat "PL" (rtos bptk 2 0) "x230..230 Lg"))
				
				(command "dim1" "hor" dm138 dm139 (polar dm139 (DTR 90.0) 79) newstud1_platex1)
				(command "dim1" "hor" dm139 dm140 (polar dm139 (DTR 90.0) 79) newstud1_platex2)
				(command "dim1" "ver" dm141 dm142 (polar dm142 (DTR 180.0) 79) newstud1_platey1)
				(command "dim1" "ver" dm142 dm143 (polar dm142 (DTR 180.0) 79) newstud1_platey2)
				(command "_insert" "f3x" dm144 "0.0394" "0.0394" "" "N2" "L150x150x12..150 Lg")
				
				;BOM
				(command "_insert" "c:/detail/bolckss/IN-7.5M-RIT-SYAL&ASS-MP_P_SOL4_boq" dm145 "" "" "" )
				(setq N1 (* (/ bptk 1000.0 )(/ 230 1000.0) (/ 230 1000.0) 7850.0 3))
				(setq N2 (* (/ 150 1000.0) 22.9 3))
				(setq N3 (* (/ (Number_Round (+ newstud1_memlen 50) 1) 1000.0) newstud1_blockwt 3))
				(setq N4 (* (/ (Number_Round (+ newst1_newmem1_len 50) 1) 1000.0) newst1_newmem1_blockwt 3))
				(setq N5 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ (+ (+ newst1_size 10) 65) 1000.0) 7850.0 4))
				(setq N6 (* (/ (Number_Round (+ newst1_newmem2_len 50) 1) 1000.0) newst1_newmem2_blockwt 3))
				(setq L11_1 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newstud1_clmapx2 1000.0) 7850.0))
				(setq L11_2 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newstud1_clmapx2 1000.0) 7850.0))
				(setq L11_3 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 150 1000.0) 7850.0 3))
				(setq L12_1 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newst1_newmem1_pclampx2 1000.0) 7850.0 2))
				(setq L12_2 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newst1_newmem1_pclampx2 1000.0) 7850.0 2))
				(setq L12_3 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 100 1000.0) 7850.0 6))
				(setq total_weight1 (+ N1 N2 N3 N4 N5 N6 L11_1 L11_2 L11_3 L12_1 L12_2 L12_3))
				(setq concrete_vol (* (/ newstud1_pdwt 1000.0) (/ newstud1_pdwt 1000.0) (/ newstud1_pdht 1000.0) 2))
				(command "style" "TRB" "Trebuchet MS" (* 2.0 sf) "1" "" "" "")
				
				(setq bom1t (polar dm145 (DTR 180.0) 1956.58))  (setq bom1 (polar bom1t (DTR 270.0) 175.8))  ;jj1
				(setq bom2t (polar dm145 (DTR 180.0) 1678.95))  (setq bom2 (polar bom2t (DTR 270.0) 175.8))  ;jj2
				(setq bom3t (polar dm145 (DTR 180.0) 1342.11))  (setq bom3 (polar bom3t (DTR 270.0) 174.96))  ;jj3
				(setq bom4t (polar dm145 (DTR 180.0) 909.5))  (setq bom4 (polar bom4t (DTR 270.0) 178.32))  ;jj4
				(setq bom5t (polar dm145 (DTR 180.0) 550.82))  (setq bom5 (polar bom5t (DTR 270.0) 178.32))  ;jj5
				(setq bom6t (polar dm145 (DTR 180.0) 1956.58))  (setq bom6 (polar bom6t (DTR 270.0) 239.22))  ;jj6
				(setq bom7t (polar dm145 (DTR 180.0) 1678.95))  (setq bom7 (polar bom7t (DTR 270.0) 238.38))  ;jj7
				(setq bom8t (polar dm145 (DTR 180.0) 1342.11))  (setq bom8 (polar bom8t (DTR 270.0) 238.38))  ;jj8
				(setq bom9t (polar dm145 (DTR 180.0) 909.5))  (setq bom9 (polar bom9t (DTR 270.0) 238.38))  ;jj9
				(setq bom10t (polar dm145 (DTR 180.0) 550.82))  (setq bom10 (polar bom10t (DTR 270.0) 241.74))  ;jj10
				(setq bom11t (polar dm145 (DTR 180.0) 1956.58))  (setq bom11 (polar bom11t (DTR 270.0) 300.12))  ;jj11
				(setq bom12t (polar dm145 (DTR 180.0) 1678.95))  (setq bom12 (polar bom12t (DTR 270.0) 302.64))  ;jj12
				(setq bom13t (polar dm145 (DTR 180.0) 1342.11))  (setq bom13 (polar bom13t (DTR 270.0) 302.64))  ;jj13
				(setq bom14t (polar dm145 (DTR 180.0) 909.5))  (setq bom14 (polar bom14t (DTR 270.0) 303.9))  ;jj14
				(setq bom15t (polar dm145 (DTR 180.0) 550.82))  (setq bom15 (polar bom15t (DTR 270.0) 303.9))  ;jj15
				(setq bom16t (polar dm145 (DTR 180.0) 1956.58))  (setq bom16 (polar bom16t (DTR 270.0) 363.54))  ;jj16
				(setq bom17t (polar dm145 (DTR 180.0) 1678.95))  (setq bom17 (polar bom17t (DTR 270.0) 362.7))  ;jj17
				(setq bom18t (polar dm145 (DTR 180.0) 1342.11))  (setq bom18 (polar bom18t (DTR 270.0) 362.7))  ;jj18
				(setq bom19t (polar dm145 (DTR 180.0) 909.5))  (setq bom19 (polar bom19t (DTR 270.0) 366.06))  ;jj19
				(setq bom20t (polar dm145 (DTR 180.0) 550.82))  (setq bom20 (polar bom20t (DTR 270.0) 366.06))  ;jj20
				(setq bom21t (polar dm145 (DTR 180.0) 1956.58))  (setq bom21 (polar bom21t (DTR 270.0) 426.96))  ;jj21
				(setq bom22t (polar dm145 (DTR 180.0) 1678.95))  (setq bom22 (polar bom22t (DTR 270.0) 426.12))  ;jj22
				(setq bom23t (polar dm145 (DTR 180.0) 1342.11))  (setq bom23 (polar bom23t (DTR 270.0) 426.12))  ;jj23
				(setq bom24t (polar dm145 (DTR 180.0) 909.5))  (setq bom24 (polar bom24t (DTR 270.0) 429.48))  ;jj24
				(setq bom25t (polar dm145 (DTR 180.0) 550.82))  (setq bom25 (polar bom25t (DTR 270.0) 429.48))  ;jj25
				(setq bom26t (polar dm145 (DTR 180.0) 1956.58))  (setq bom26 (polar bom26t (DTR 270.0) 487.86))  ;jj26
				(setq bom27t (polar dm145 (DTR 180.0) 1678.95))  (setq bom27 (polar bom27t (DTR 270.0) 490.38))  ;jj27
				(setq bom28t (polar dm145 (DTR 180.0) 1342.11))  (setq bom28 (polar bom28t (DTR 270.0) 490.38))  ;jj28
				(setq bom29t (polar dm145 (DTR 180.0) 909.5))  (setq bom29 (polar bom29t (DTR 270.0) 491.64))  ;jj29
				(setq bom30t (polar dm145 (DTR 180.0) 550.82))  (setq bom30 (polar bom30t (DTR 270.0) 491.64))  ;jj30
				(setq bom31t (polar dm145 (DTR 180.0) 1928.04))  (setq bom31 (polar bom31t (DTR 270.0) 547.92))  ;jj31
				(setq bom32t (polar dm145 (DTR 180.0) 1678.95))  (setq bom32 (polar bom32t (DTR 270.0) 551.28))  ;jj32
				(setq bom33t (polar dm145 (DTR 180.0) 1342.11))  (setq bom33 (polar bom33t (DTR 270.0) 551.28))  ;jj33
				(setq bom34t (polar dm145 (DTR 180.0) 909.5))  (setq bom34 (polar bom34t (DTR 270.0) 552.54))  ;jj34
				(setq bom35t (polar dm145 (DTR 180.0) 550.82))  (setq bom35 (polar bom35t (DTR 270.0) 552.54))  ;jj35
				(setq bom36t (polar dm145 (DTR 180.0) 1931.05))  (setq bom36 (polar bom36t (DTR 270.0) 612.19))  ;jj36
				(setq bom37t (polar dm145 (DTR 180.0) 1678.95))  (setq bom37 (polar bom37t (DTR 270.0) 612.19))  ;jj37
				(setq bom38t (polar dm145 (DTR 180.0) 1342.11))  (setq bom38 (polar bom38t (DTR 270.0) 612.19))  ;jj38
				(setq bom39t (polar dm145 (DTR 180.0) 919.58))  (setq bom39 (polar bom39t (DTR 270.0) 613.45))  ;jj39
				(setq bom40t (polar dm145 (DTR 180.0) 560.9))  (setq bom40 (polar bom40t (DTR 270.0) 613.45))  ;jj40
				(setq bom41t (polar dm145 (DTR 180.0) 1931.05))  (setq bom41 (polar bom41t (DTR 270.0) 673.09))  ;jj41
				(setq bom42t (polar dm145 (DTR 180.0) 1687.95))  (setq bom42 (polar bom42t (DTR 270.0) 673.09))  ;jj42
				(setq bom43t (polar dm145 (DTR 180.0) 1342.11))  (setq bom43 (polar bom43t (DTR 270.0) 673.09))  ;jj43
				(setq bom44t (polar dm145 (DTR 180.0) 929.66))  (setq bom44 (polar bom44t (DTR 270.0) 674.35))  ;jj44
				(setq bom45t (polar dm145 (DTR 180.0) 570.98))  (setq bom45 (polar bom45t (DTR 270.0) 674.35))  ;jj45
				(setq bom46t (polar dm145 (DTR 180.0) 1931.05))  (setq bom46 (polar bom46t (DTR 270.0) 733.15))  ;jj46
				(setq bom47t (polar dm145 (DTR 180.0) 1682.91))  (setq bom47 (polar bom47t (DTR 270.0) 733.15))  ;jj47
				(setq bom48t (polar dm145 (DTR 180.0) 1342.11))  (setq bom48 (polar bom48t (DTR 270.0) 733.15))  ;jj48
				(setq bom49t (polar dm145 (DTR 180.0) 909.5))  (setq bom49 (polar bom49t (DTR 270.0) 736.51))  ;jj49
				(setq bom50t (polar dm145 (DTR 180.0) 550.82))  (setq bom50 (polar bom50t (DTR 270.0) 736.51))  ;jj50
				(setq bom51t (polar dm145 (DTR 180.0) 1931.05))  (setq bom51 (polar bom51t (DTR 270.0) 797.41))  ;jj51
				(setq bom52t (polar dm145 (DTR 180.0) 1678.95))  (setq bom52 (polar bom52t (DTR 270.0) 797.41))  ;jj52
				(setq bom53t (polar dm145 (DTR 180.0) 1342.11))  (setq bom53 (polar bom53t (DTR 270.0) 797.41))  ;jj53
				(setq bom54t (polar dm145 (DTR 180.0) 909.5))  (setq bom54 (polar bom54t (DTR 270.0) 798.67))  ;jj54
				(setq bom55t (polar dm145 (DTR 180.0) 550.82))  (setq bom55 (polar bom55t (DTR 270.0) 798.67))  ;jj55
				(setq bom56t (polar dm145 (DTR 180.0) 1931.05))  (setq bom56 (polar bom56t (DTR 270.0) 858.31))  ;jj56
				(setq bom57t (polar dm145 (DTR 180.0) 1678.95))  (setq bom57 (polar bom57t (DTR 270.0) 858.31))  ;jj57
				(setq bom58t (polar dm145 (DTR 180.0) 1342.11))  (setq bom58 (polar bom58t (DTR 270.0) 858.31))  ;jj58
				(setq bom59t (polar dm145 (DTR 180.0) 909.5))  (setq bom59 (polar bom59t (DTR 270.0) 859.57))  ;jj59
				(setq bom60t (polar dm145 (DTR 180.0) 550.82))  (setq bom60 (polar bom60t (DTR 270.0) 859.57))  ;jj60
				(setq bom61t (polar dm145 (DTR 180.0) 941.84))  (setq bom61 (polar bom61t (DTR 270.0) 921.73))  ;jj61
				(setq bom62t (polar dm145 (DTR 180.0) 583.16))  (setq bom62 (polar bom62t (DTR 270.0) 921.73))  ;jj62
				(setq bom63t (polar dm145 (DTR 180.0) 953.6))  (setq bom63 (polar bom63t (DTR 270.0) 1403.89))  ;jj63
				(setq bom64t (polar dm145 (DTR 180.0) 594.92))  (setq bom64 (polar bom64t (DTR 270.0) 1403.89))  ;jj64
				(setq bom65t (polar dm145 (DTR 180.0) 953.6))  (setq bom65 (polar bom65t (DTR 270.0) 1535.77))  ;jj65
				(setq bom66t (polar dm145 (DTR 180.0) 594.92))  (setq bom66 (polar bom66t (DTR 270.0) 1535.77))  ;jj66
				(setq bom67t (polar dm145 (DTR 180.0) 670.75))  (setq bom67 (polar bom67t (DTR 270.0) 1593.5))  ;jj67
				
				(command "TEXT" bom1 "0" "L11-1")
				(command "TEXT" bom2 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom3 "0" (rtos newstud1_clmapx2 2 0))
				(command "TEXT" bom4 "0" (rtos L11_1 2 2))
				(command "TEXT" bom5 "0" (rtos (* 1.035 L11_1) 2 2))
				(command "TEXT" bom6 "0" "L11-2")
				(command "TEXT" bom7 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom8 "0" (rtos newstud1_clmapx2 2 0))
				(command "TEXT" bom9 "0" (rtos L11_2 2 2))
				(command "TEXT" bom10 "0" (rtos (* 1.035 L11_2) 2 2))
				(command "TEXT" bom11 "0" "L11-3")
				(command "TEXT" bom12 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x100" ))
				(command "TEXT" bom13 "0" "150")
				(command "TEXT" bom14 "0" (rtos L11_3 2 2))
				(command "TEXT" bom15 "0" (rtos (* 1.035 L11_3 ) 2 2))
				
				(command "TEXT" bom16 "0" "L12-1")
				(command "TEXT" bom17 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom18 "0" (rtos newst1_newmem1_pclampx2 2 0))
				(command "TEXT" bom19 "0" (rtos L12_1 2 2))
				(command "TEXT" bom20 "0" (rtos (* 1.035 L12_1) 2 2))
				(command "TEXT" bom21 "0" "L12-2")
				(command "TEXT" bom22 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom23 "0" (rtos newst1_newmem1_pclampx2 2 0))
				(command "TEXT" bom24 "0" (rtos L12_2 2 2))
				(command "TEXT" bom25 "0" (rtos (* 1.035 L12_2) 2 2))
				(command "TEXT" bom26 "0" "L12-3")
				(command "TEXT" bom27 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x100" ))
				(command "TEXT" bom28 "0" "100")
				(command "TEXT" bom29 "0" (rtos L12_3 2 2))
				(command "TEXT" bom30 "0" (rtos (* 1.035 L12_3 ) 2 2))
				
				(command "TEXT" bom31 "0" "N1")
				(command "TEXT" bom32 "0" (strcat "PL" (rtos bptk 2 0) "x230"))
				(command "TEXT" bom33 "0" "230")
				(command "TEXT" bom34 "0" (rtos N1 2 2))
				(command "TEXT" bom35 "0" (rtos (* 1.035 N1) 2 2))
				(command "TEXT" bom36 "0" "N2")
				(command "TEXT" bom37 "0" "L150x10")
				(command "TEXT" bom38 "0" "150")
				(command "TEXT" bom39 "0" (rtos N2 2 2))
				(command "TEXT" bom40 "0" (rtos (* 1.035 N2) 2 2))
				(command "TEXT" bom41 "0" "N3")
				(command "TEXT" bom42 "0" newstud1_memname)
				(command "TEXT" bom43 "0" (itoa (Number_Round (+ newstud1_memlen 50) 1)))
				(command "TEXT" bom44 "0" (rtos N3 2 2))
				(command "TEXT" bom45 "0" (rtos (* 1.035 N3 ) 2 2))
				(command "TEXT" bom46 "0" "N4")
				(command "TEXT" bom47 "0" newst1_newmem1_memname)
				(command "TEXT" bom48 "0" (itoa (Number_Round (+ newst1_newmem1_len 50) 1)))
				(command "TEXT" bom49 "0" (rtos N4 2 2))
				(command "TEXT" bom50 "0" (rtos (* 1.035 N4 ) 2 2))
				(command "TEXT" bom51 "0" "N5")
				(command "TEXT" bom52 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x100"))
				(command "TEXT" bom53 "0" (itoa (Number_Round (+ newst1_newmem1_len 50) 1)))
				(command "TEXT" bom54 "0" (rtos N5 2 2))
				(command "TEXT" bom55 "0" (rtos (* 1.035 N5 ) 2 2))
				(command "TEXT" bom56 "0" "N6")
				(command "TEXT" bom57 "0" newst1_newmem2_memname)
				(command "TEXT" bom58 "0" (itoa (Number_Round (+ newst1_newmem2_len 50) 1)))
				(command "TEXT" bom59 "0" (rtos N6 2 2))
				(command "TEXT" bom60 "0" (rtos (* 1.035 N6 ) 2 2))
				
				(command "TEXT" bom61 "0" (rtos total_weight1 2 2))
				(command "TEXT" bom62 "0" (rtos (* total_weight1 1.035) 2 2))
				(command "TEXT" bom67 "0" (rtos concrete_vol 2 3))
				(command "style" "WMF-Trebuchet MS0" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" bom63 "0" (rtos total_weight1 2 2))
				(command "TEXT" bom64 "0" (rtos (* total_weight1 1.035) 2 2))
				(command "TEXT" bom65 "0" (rtos (+ total_weight1 7.76) 2 2))
				(command "TEXT" bom66 "0" (rtos (* (+ total_weight1 7.76) 1.035) 2 2))
				
				(setq next1 (polar ibp (dtr 0.0) 9900))
				(setq ibp next1)
		))
		
		(if (and (= newstudnum 1) (= newstud1_memty "P") (= newst_mem_onpole 2) (= newst_mem_onstruct 2) (= newst1_newmem1_type "P") (= hip_mem 2) (= hip_mem1_type "P") ) (progn
				(if (= ibp nil) (setq ibp '(0.0 0.0 0.0)))
				(if (= polenumber nil) (setq polenumber 1) (setq polenumber (1+ polenumber)))
				(command "_insert" "c:/detail/bolckss/IN-6.0M-RIT-CNTR-MP_P_SOL5" ibp "" "" "" )
				
				(setq dm1t (polar ibp (DTR 0.0) 1019.55))  (setq dm1 (polar dm1t (DTR 90.0) 2079.97))  ;K1
				(setq dm2t (polar ibp (DTR 0.0) 1381.08))  (setq dm2 (polar dm2t (DTR 90.0) 2329.97))  ;K2
				(setq dm3t (polar ibp (DTR 0.0) 882.31))  (setq dm3 (polar dm3t (DTR 90.0) 2079.97))  ;K3
				(setq dm4t (polar ibp (DTR 0.0) 1426.08))  (setq dm4 (polar dm4t (DTR 90.0) 3953.72))  ;K4
				(setq dm5t (polar ibp (DTR 0.0) 1443.58))  (setq dm5 (polar dm5t (DTR 90.0) 5829.97))  ;K5
				(setq dm6t (polar ibp (DTR 0.0) 825.3))  (setq dm6 (polar dm6t (DTR 90.0) 2079.97))  ;K6
				(setq dm7t (polar ibp (DTR 0.0) 772.69))  (setq dm7 (polar dm7t (DTR 90.0) 2936.52))  ;K7
				(setq dm8t (polar ibp (DTR 0.0) 772.69))  (setq dm8 (polar dm8t (DTR 90.0) 3903.65))  ;K8
				(setq dm9t (polar ibp (DTR 0.0) 750.06))  (setq dm9 (polar dm9t (DTR 90.0) 4891.84))  ;K9
				(setq dm10t (polar ibp (DTR 0.0) 825.3))  (setq dm10 (polar dm10t (DTR 90.0) 5829.97))  ;K10
				(setq dm11t (polar ibp (DTR 0.0) 635.03))  (setq dm11 (polar dm11t (DTR 90.0) 2079.97))  ;K11
				(setq dm12t (polar ibp (DTR 0.0) 635.03))  (setq dm12 (polar dm12t (DTR 90.0) 5829.97))  ;K12
				(setq dm13t (polar ibp (DTR 0.0) 1506.08))  (setq dm13 (polar dm13t (DTR 90.0) 1877.73))  ;K13
				(setq dm14t (polar ibp (DTR 0.0) 2443.62))  (setq dm14 (polar dm14t (DTR 90.0) 1877.73))  ;K14
				(setq dm15t (polar ibp (DTR 0.0) 2631.08))  (setq dm15 (polar dm15t (DTR 90.0) 1877.73))  ;K15
				(setq dm16t (polar ibp (DTR 0.0) 1056.24))  (setq dm16 (polar dm16t (DTR 90.0) 3215.91))  ;K16
				(setq dm17t (polar ibp (DTR 0.0) 1056.24))  (setq dm17 (polar dm17t (DTR 90.0) 5216.67))  ;K17
				(setq dm18t (polar ibp (DTR 0.0) 1882.37))  (setq dm18 (polar dm18t (DTR 90.0) 2538.66))  ;K18
				(setq dm19t (polar ibp (DTR 0.0) 2989.11))  (setq dm19 (polar dm19t (DTR 90.0) 2000.29))  ;K19
				(setq dm20t (polar ibp (DTR 0.0) 1795.48))  (setq dm20 (polar dm20t (DTR 90.0) 1508.42))  ;K20
				(setq dm21t (polar ibp (DTR 0.0) 942.57))  (setq dm21 (polar dm21t (DTR 90.0) 899.64))  ;K21
				(setq dm22t (polar ibp (DTR 0.0) 1617.57))  (setq dm22 (polar dm22t (DTR 90.0) 899.64))  ;K22
				(setq dm23t (polar ibp (DTR 0.0) 2672.03))  (setq dm23 (polar dm23t (DTR 90.0) 917.85))  ;K23
				(setq dm24t (polar ibp (DTR 0.0) 3428.63))  (setq dm24 (polar dm24t (DTR 90.0) 916.63))  ;K24
				(setq dm25t (polar ibp (DTR 0.0) 1787.73))  (setq dm25 (polar dm25t (DTR 90.0) 2963.34))  ;K25
				(setq dm26t (polar ibp (DTR 0.0) 1654.53))  (setq dm26 (polar dm26t (DTR 90.0) 3791.67))  ;K26
				(setq dm27t (polar ibp (DTR 0.0) 4624.34))  (setq dm27 (polar dm27t (DTR 90.0) 3625.21))  ;K27
				(setq dm28t (polar ibp (DTR 0.0) 4897.48))  (setq dm28 (polar dm28t (DTR 90.0) 3625.21))  ;K28
				(setq dm29t (polar ibp (DTR 0.0) 4508.16))  (setq dm29 (polar dm29t (DTR 90.0) 3421.37))  ;K29
				(setq dm30t (polar ibp (DTR 0.0) 5013.66))  (setq dm30 (polar dm30t (DTR 90.0) 3421.37))  ;K30
				(setq dm31t (polar ibp (DTR 0.0) 4466.55))  (setq dm31 (polar dm31t (DTR 90.0) 3222.67))  ;K31
				(setq dm32t (polar ibp (DTR 0.0) 5366.83))  (setq dm32 (polar dm32t (DTR 90.0) 3609.71))  ;K32
				(setq dm33t (polar ibp (DTR 0.0) 5644.41))  (setq dm33 (polar dm33t (DTR 90.0) 3609.71))  ;K33
				(setq dm34t (polar ibp (DTR 0.0) 5248.76))  (setq dm34 (polar dm34t (DTR 90.0) 3402.56))  ;K34
				(setq dm35t (polar ibp (DTR 0.0) 5762.48))  (setq dm35 (polar dm35t (DTR 90.0) 3402.56))  ;K35
				(setq dm36t (polar ibp (DTR 0.0) 5200.33))  (setq dm36 (polar dm36t (DTR 90.0) 3216.9))  ;K36
				(setq dm37t (polar ibp (DTR 0.0) 6127.03))  (setq dm37 (polar dm37t (DTR 90.0) 3607.8))  ;K37
				(setq dm38t (polar ibp (DTR 0.0) 6210.43))  (setq dm38 (polar dm38t (DTR 90.0) 3607.8))  ;K38
				(setq dm39t (polar ibp (DTR 0.0) 6298.82))  (setq dm39 (polar dm39t (DTR 90.0) 3607.8))  ;K39
				(setq dm40t (polar ibp (DTR 0.0) 6127.48))  (setq dm40 (polar dm40t (DTR 90.0) 3532.8))  ;K40
				(setq dm41t (polar ibp (DTR 0.0) 6127.03))  (setq dm41 (polar dm41t (DTR 90.0) 3464.71))  ;K41
				(setq dm42t (polar ibp (DTR 0.0) 5879.28))  (setq dm42 (polar dm42t (DTR 90.0) 3204.08))  ;K42
				(setq dm43t (polar ibp (DTR 0.0) 6074.02))  (setq dm43 (polar dm43t (DTR 90.0) 3940.73))  ;K43
				(setq dm44t (polar ibp (DTR 0.0) 6157.67))  (setq dm44 (polar dm44t (DTR 90.0) 3966.16))  ;K44
				(setq dm45t (polar ibp (DTR 0.0) 6396.27))  (setq dm45 (polar dm45t (DTR 90.0) 3943.07))  ;K45
				(setq dm46t (polar ibp (DTR 0.0) 6396.27))  (setq dm46 (polar dm46t (DTR 90.0) 3958.46))  ;K46
				(setq dm47t (polar ibp (DTR 0.0) 6074.02))  (setq dm47 (polar dm47t (DTR 90.0) 4240.55))  ;K47
				(setq dm48t (polar ibp (DTR 0.0) 6157.67))  (setq dm48 (polar dm48t (DTR 90.0) 4265.97))  ;K48
				(setq dm49t (polar ibp (DTR 0.0) 6396.27))  (setq dm49 (polar dm49t (DTR 90.0) 4242.89))  ;K49
				(setq dm50t (polar ibp (DTR 0.0) 6396.27))  (setq dm50 (polar dm50t (DTR 90.0) 4258.28))  ;K50
				(setq dm51t (polar ibp (DTR 0.0) 4624.34))  (setq dm51 (polar dm51t (DTR 90.0) 2273.13))  ;K51
				(setq dm52t (polar ibp (DTR 0.0) 4897.48))  (setq dm52 (polar dm52t (DTR 90.0) 2273.13))  ;K52
				(setq dm53t (polar ibp (DTR 0.0) 4508.16))  (setq dm53 (polar dm53t (DTR 90.0) 2069.3))  ;K53
				(setq dm54t (polar ibp (DTR 0.0) 5013.66))  (setq dm54 (polar dm54t (DTR 90.0) 2069.3))  ;K54
				(setq dm55t (polar ibp (DTR 0.0) 4466.55))  (setq dm55 (polar dm55t (DTR 90.0) 1870.6))  ;K55
				(setq dm56t (polar ibp (DTR 0.0) 5366.83))  (setq dm56 (polar dm56t (DTR 90.0) 2257.64))  ;K56
				(setq dm57t (polar ibp (DTR 0.0) 5644.41))  (setq dm57 (polar dm57t (DTR 90.0) 2257.64))  ;K57
				(setq dm58t (polar ibp (DTR 0.0) 5248.76))  (setq dm58 (polar dm58t (DTR 90.0) 2050.49))  ;K58
				(setq dm59t (polar ibp (DTR 0.0) 5762.48))  (setq dm59 (polar dm59t (DTR 90.0) 2050.49))  ;K59
				(setq dm60t (polar ibp (DTR 0.0) 5200.33))  (setq dm60 (polar dm60t (DTR 90.0) 1864.83))  ;K60
				(setq dm61t (polar ibp (DTR 0.0) 5885.71))  (setq dm61 (polar dm61t (DTR 90.0) 1873.37))  ;K61
				(setq dm62t (polar ibp (DTR 0.0) 6064.02))  (setq dm62 (polar dm62t (DTR 90.0) 2618.65))  ;K62
				(setq dm63t (polar ibp (DTR 0.0) 6147.67))  (setq dm63 (polar dm63t (DTR 90.0) 2644.08))  ;K63
				(setq dm64t (polar ibp (DTR 0.0) 6386.27))  (setq dm64 (polar dm64t (DTR 90.0) 2621))  ;K64
				(setq dm65t (polar ibp (DTR 0.0) 6386.27))  (setq dm65 (polar dm65t (DTR 90.0) 2636.39))  ;K65
				(setq dm66t (polar ibp (DTR 0.0) 6064.02))  (setq dm66 (polar dm66t (DTR 90.0) 2918.47))  ;K66
				(setq dm67t (polar ibp (DTR 0.0) 6147.67))  (setq dm67 (polar dm67t (DTR 90.0) 2943.9))  ;K67
				(setq dm68t (polar ibp (DTR 0.0) 6386.27))  (setq dm68 (polar dm68t (DTR 90.0) 2920.82))  ;K68
				(setq dm69t (polar ibp (DTR 0.0) 6386.27))  (setq dm69 (polar dm69t (DTR 90.0) 2936.21))  ;K69
				(setq dm70t (polar ibp (DTR 0.0) 6803.04))  (setq dm70 (polar dm70t (DTR 90.0) 5792.65))  ;K70
				(setq dm71t (polar ibp (DTR 0.0) 6803.82))  (setq dm71 (polar dm71t (DTR 90.0) 5872.7))  ;K71
				(setq dm72t (polar ibp (DTR 0.0) 6803.82))  (setq dm72 (polar dm72t (DTR 90.0) 5952.71))  ;K72
				(setq dm73t (polar ibp (DTR 0.0) 7032.86))  (setq dm73 (polar dm73t (DTR 90.0) 5952.71))  ;K73
				(setq dm74t (polar ibp (DTR 0.0) 7212.86))  (setq dm74 (polar dm74t (DTR 90.0) 5933.01))  ;K74
				(setq dm75t (polar ibp (DTR 0.0) 8223.68))  (setq dm75 (polar dm75t (DTR 90.0) 5932.95))  ;K75
				(setq dm76t (polar ibp (DTR 0.0) 8403.68))  (setq dm76 (polar dm76t (DTR 90.0) 5952.65))  ;K76
				(setq dm77t (polar ibp (DTR 0.0) 8463.68))  (setq dm77 (polar dm77t (DTR 90.0) 5792.65))  ;K77
				(setq dm78t (polar ibp (DTR 0.0) 8223.68))  (setq dm78 (polar dm78t (DTR 90.0) 5812.35))  ;K78
				(setq dm79t (polar ibp (DTR 0.0) 7972.69))  (setq dm79 (polar dm79t (DTR 90.0) 5812.4))  ;K79
				(setq dm80t (polar ibp (DTR 0.0) 7517.69))  (setq dm80 (polar dm80t (DTR 90.0) 5812.4))  ;K80
				(setq dm81t (polar ibp (DTR 0.0) 7212.86))  (setq dm81 (polar dm81t (DTR 90.0) 5812.41))  ;K81
				(setq dm82t (polar ibp (DTR 0.0) 7659.17))  (setq dm82 (polar dm82t (DTR 90.0) 5812.41))  ;K82
				(setq dm83t (polar ibp (DTR 0.0) 7655.43))  (setq dm83 (polar dm83t (DTR 90.0) 5933.01))  ;K83
				(setq dm84t (polar ibp (DTR 0.0) 6972.08))  (setq dm84 (polar dm84t (DTR 90.0) 5792.65))  ;K84
				(setq dm85t (polar ibp (DTR 0.0) 7318.64))  (setq dm85 (polar dm85t (DTR 90.0) 5528.14))  ;K85
				(setq dm86t (polar ibp (DTR 0.0) 6836.94))  (setq dm86 (polar dm86t (DTR 90.0) 5192))  ;K86
				(setq dm87t (polar ibp (DTR 0.0) 6836.94))  (setq dm87 (polar dm87t (DTR 90.0) 5261))  ;K87
				(setq dm88t (polar ibp (DTR 0.0) 6836.94))  (setq dm88 (polar dm88t (DTR 90.0) 5330))  ;K88
				(setq dm89t (polar ibp (DTR 0.0) 6966.94))  (setq dm89 (polar dm89t (DTR 90.0) 5330))  ;K89
				(setq dm90t (polar ibp (DTR 0.0) 7116.94))  (setq dm90 (polar dm90t (DTR 90.0) 5309.3))  ;K90
				(setq dm91t (polar ibp (DTR 0.0) 7807.62))  (setq dm91 (polar dm91t (DTR 90.0) 5309.3))  ;K91
				(setq dm92t (polar ibp (DTR 0.0) 7957.62))  (setq dm92 (polar dm92t (DTR 90.0) 5330))  ;K92
				(setq dm93t (polar ibp (DTR 0.0) 8037.62))  (setq dm93 (polar dm93t (DTR 90.0) 5192))  ;K93
				(setq dm94t (polar ibp (DTR 0.0) 7807.62))  (setq dm94 (polar dm94t (DTR 90.0) 5212.7))  ;K94
				(setq dm95t (polar ibp (DTR 0.0) 7116.94))  (setq dm95 (polar dm95t (DTR 90.0) 5212.7))  ;K95
				(setq dm96t (polar ibp (DTR 0.0) 6886.94))  (setq dm96 (polar dm96t (DTR 90.0) 5192))  ;K96
				(setq dm97t (polar ibp (DTR 0.0) 7389.92))  (setq dm97 (polar dm97t (DTR 90.0) 5212.7))  ;K97
				(setq dm98t (polar ibp (DTR 0.0) 7389.92))  (setq dm98 (polar dm98t (DTR 90.0) 5309.3))  ;K98
				(setq dm99t (polar ibp (DTR 0.0) 7073.11))  (setq dm99 (polar dm99t (DTR 90.0) 4900.83))  ;K99
				(setq dm100t (polar ibp (DTR 0.0) 6836.94))  (setq dm100 (polar dm100t (DTR 90.0) 4533.54))  ;K100
				(setq dm101t (polar ibp (DTR 0.0) 6836.94))  (setq dm101 (polar dm101t (DTR 90.0) 4602.54))  ;K101
				(setq dm102t (polar ibp (DTR 0.0) 6836.94))  (setq dm102 (polar dm102t (DTR 90.0) 4671.54))  ;K102
				(setq dm103t (polar ibp (DTR 0.0) 6966.94))  (setq dm103 (polar dm103t (DTR 90.0) 4671.54))  ;K103
				(setq dm104t (polar ibp (DTR 0.0) 7116.94))  (setq dm104 (polar dm104t (DTR 90.0) 4650.84))  ;K104
				(setq dm105t (polar ibp (DTR 0.0) 7807.62))  (setq dm105 (polar dm105t (DTR 90.0) 4650.84))  ;K105
				(setq dm106t (polar ibp (DTR 0.0) 7957.62))  (setq dm106 (polar dm106t (DTR 90.0) 4671.54))  ;K106
				(setq dm107t (polar ibp (DTR 0.0) 8037.62))  (setq dm107 (polar dm107t (DTR 90.0) 4533.54))  ;K107
				(setq dm108t (polar ibp (DTR 0.0) 7807.62))  (setq dm108 (polar dm108t (DTR 90.0) 4554.24))  ;K108
				(setq dm109t (polar ibp (DTR 0.0) 7116.94))  (setq dm109 (polar dm109t (DTR 90.0) 4554.24))  ;K109
				(setq dm110t (polar ibp (DTR 0.0) 6886.94))  (setq dm110 (polar dm110t (DTR 90.0) 4533.54))  ;K110
				(setq dm111t (polar ibp (DTR 0.0) 7389.92))  (setq dm111 (polar dm111t (DTR 90.0) 4554.24))  ;K111
				(setq dm112t (polar ibp (DTR 0.0) 7389.92))  (setq dm112 (polar dm112t (DTR 90.0) 4650.84))  ;K112
				(setq dm113t (polar ibp (DTR 0.0) 7073.11))  (setq dm113 (polar dm113t (DTR 90.0) 4242.37))  ;K113
				(setq dm114t (polar ibp (DTR 0.0) 3797.31))  (setq dm114 (polar dm114t (DTR 90.0) 517.3))  ;K114
				(setq dm115t (polar ibp (DTR 0.0) 3797.31))  (setq dm115 (polar dm115t (DTR 90.0) 586.3))  ;K115
				(setq dm116t (polar ibp (DTR 0.0) 3797.31))  (setq dm116 (polar dm116t (DTR 90.0) 655.3))  ;K116
				(setq dm117t (polar ibp (DTR 0.0) 3927.31))  (setq dm117 (polar dm117t (DTR 90.0) 661.3))  ;K117
				(setq dm118t (polar ibp (DTR 0.0) 4077.31))  (setq dm118 (polar dm118t (DTR 90.0) 634.6))  ;K118
				(setq dm119t (polar ibp (DTR 0.0) 4767.99))  (setq dm119 (polar dm119t (DTR 90.0) 634.6))  ;K119
				(setq dm120t (polar ibp (DTR 0.0) 4917.99))  (setq dm120 (polar dm120t (DTR 90.0) 655.3))  ;K120
				(setq dm121t (polar ibp (DTR 0.0) 4997.99))  (setq dm121 (polar dm121t (DTR 90.0) 517.3))  ;K121
				(setq dm122t (polar ibp (DTR 0.0) 4767.99))  (setq dm122 (polar dm122t (DTR 90.0) 538))  ;K122
				(setq dm123t (polar ibp (DTR 0.0) 4077.31))  (setq dm123 (polar dm123t (DTR 90.0) 538))  ;K123
				(setq dm124t (polar ibp (DTR 0.0) 3847.31))  (setq dm124 (polar dm124t (DTR 90.0) 517.3))  ;K124
				(setq dm125t (polar ibp (DTR 0.0) 4350.29))  (setq dm125 (polar dm125t (DTR 90.0) 538))  ;K125
				(setq dm126t (polar ibp (DTR 0.0) 4350.29))  (setq dm126 (polar dm126t (DTR 90.0) 634.6))  ;K126
				(setq dm127t (polar ibp (DTR 0.0) 4033.48))  (setq dm127 (polar dm127t (DTR 90.0) 226.13))  ;K127
				(setq dm128t (polar ibp (DTR 0.0) 5280.86))  (setq dm128 (polar dm128t (DTR 90.0) 517.3))  ;K128
				(setq dm129t (polar ibp (DTR 0.0) 5280.86))  (setq dm129 (polar dm129t (DTR 90.0) 586.3))  ;K129
				(setq dm130t (polar ibp (DTR 0.0) 5280.86))  (setq dm130 (polar dm130t (DTR 90.0) 655.3))  ;K130
				(setq dm131t (polar ibp (DTR 0.0) 5410.86))  (setq dm131 (polar dm131t (DTR 90.0) 655.3))  ;K131
				(setq dm132t (polar ibp (DTR 0.0) 5560.86))  (setq dm132 (polar dm132t (DTR 90.0) 634.6))  ;K132
				(setq dm133t (polar ibp (DTR 0.0) 6251.54))  (setq dm133 (polar dm133t (DTR 90.0) 634.6))  ;K133
				(setq dm134t (polar ibp (DTR 0.0) 6401.54))  (setq dm134 (polar dm134t (DTR 90.0) 655.3))  ;K134
				(setq dm135t (polar ibp (DTR 0.0) 6481.54))  (setq dm135 (polar dm135t (DTR 90.0) 517.3))  ;K135
				(setq dm136t (polar ibp (DTR 0.0) 6251.54))  (setq dm136 (polar dm136t (DTR 90.0) 538))  ;K136
				(setq dm137t (polar ibp (DTR 0.0) 5560.86))  (setq dm137 (polar dm137t (DTR 90.0) 538))  ;K137
				(setq dm138t (polar ibp (DTR 0.0) 5330.86))  (setq dm138 (polar dm138t (DTR 90.0) 517.3))  ;K138
				(setq dm139t (polar ibp (DTR 0.0) 5833.84))  (setq dm139 (polar dm139t (DTR 90.0) 538))  ;K139
				(setq dm140t (polar ibp (DTR 0.0) 5833.84))  (setq dm140 (polar dm140t (DTR 90.0) 634.6))  ;K140
				(setq dm141t (polar ibp (DTR 0.0) 5517.03))  (setq dm141 (polar dm141t (DTR 90.0) 226.13))  ;K141
				(setq dm142t (polar ibp (DTR 0.0) 8576.15))  (setq dm142 (polar dm142t (DTR 90.0) 5327.54))  ;K142
				(setq dm143t (polar ibp (DTR 0.0) 8632.67))  (setq dm143 (polar dm143t (DTR 90.0) 5327.54))  ;K143
				(setq dm144t (polar ibp (DTR 0.0) 8296.09))  (setq dm144 (polar dm144t (DTR 90.0) 5127.54))  ;K144
				(setq dm145t (polar ibp (DTR 0.0) 8632.67))  (setq dm145 (polar dm145t (DTR 90.0) 5127.54))  ;K145
				(setq dm146t (polar ibp (DTR 0.0) 8190.79))  (setq dm146 (polar dm146t (DTR 90.0) 4905.6))  ;K146
				(setq dm147t (polar ibp (DTR 0.0) 8190.79))  (setq dm147 (polar dm147t (DTR 90.0) 4247.14))  ;K147
				(setq dm148t (polar ibp (DTR 0.0) 8089.68))  (setq dm148 (polar dm148t (DTR 90.0) 3471.42))  ;K148
				(setq dm149t (polar ibp (DTR 0.0) 8314.68))  (setq dm149 (polar dm149t (DTR 90.0) 3502.47))  ;K149
				(setq dm150t (polar ibp (DTR 0.0) 8509.68))  (setq dm150 (polar dm150t (DTR 90.0) 3502.47))  ;K150
				(setq dm151t (polar ibp (DTR 0.0) 8509.68))  (setq dm151 (polar dm151t (DTR 90.0) 3398.97))  ;K151
				(setq dm152t (polar ibp (DTR 0.0) 8509.68))  (setq dm152 (polar dm152t (DTR 90.0) 3295.47))  ;K152
				(setq dm153t (polar ibp (DTR 0.0) 8434.68))  (setq dm153 (polar dm153t (DTR 90.0) 3295.47))  ;K153
				(setq dm154t (polar ibp (DTR 0.0) 8089.68))  (setq dm154 (polar dm154t (DTR 90.0) 3326.52))  ;K154
				(setq dm155t (polar ibp (DTR 0.0) 7898.2))  (setq dm155 (polar dm155t (DTR 90.0) 3471.42))  ;K155
				(setq dm156t (polar ibp (DTR 0.0) 7898.2))  (setq dm156 (polar dm156t (DTR 90.0) 3326.52))  ;K156
				(setq dm157t (polar ibp (DTR 0.0) 8043.23))  (setq dm157 (polar dm157t (DTR 90.0) 2275.41))  ;K157
				(setq dm158t (polar ibp (DTR 0.0) 8268.23))  (setq dm158 (polar dm158t (DTR 90.0) 2306.46))  ;K158
				(setq dm159t (polar ibp (DTR 0.0) 8535.22))  (setq dm159 (polar dm159t (DTR 90.0) 2306.46))  ;K159
				(setq dm160t (polar ibp (DTR 0.0) 8535.22))  (setq dm160 (polar dm160t (DTR 90.0) 2202.96))  ;K160
				(setq dm161t (polar ibp (DTR 0.0) 8535.22))  (setq dm161 (polar dm161t (DTR 90.0) 2099.46))  ;K161
				(setq dm162t (polar ibp (DTR 0.0) 8352.62))  (setq dm162 (polar dm162t (DTR 90.0) 2099.46))  ;K162
				(setq dm163t (polar ibp (DTR 0.0) 8043.23))  (setq dm163 (polar dm163t (DTR 90.0) 2130.51))  ;K163
				(setq dm164t (polar ibp (DTR 0.0) 7890.01))  (setq dm164 (polar dm164t (DTR 90.0) 2275.41))  ;K164
				(setq dm165t (polar ibp (DTR 0.0) 7890.01))  (setq dm165 (polar dm165t (DTR 90.0) 2130.51))  ;K165
				(setq dm166t (polar ibp (DTR 0.0) 3477.99))  (setq dm166 (polar dm166t (DTR 90.0) 4109.03))  ;K166
				(setq dm167t (polar ibp (DTR 0.0) 3514.91))  (setq dm167 (polar dm167t (DTR 90.0) 3868.13))  ;K167
				(setq dm168t (polar ibp (DTR 0.0) 3627.39))  (setq dm168 (polar dm168t (DTR 90.0) 3868.13))  ;K168
				(setq dm169t (polar ibp (DTR 0.0) 3762.11))  (setq dm169 (polar dm169t (DTR 90.0) 3868.13))  ;K169
				(setq dm170t (polar ibp (DTR 0.0) 3432.11))  (setq dm170 (polar dm170t (DTR 90.0) 3785.33))  ;K170
				(setq dm171t (polar ibp (DTR 0.0) 3432.11))  (setq dm171 (polar dm171t (DTR 90.0) 3672.85))  ;K171
				(setq dm172t (polar ibp (DTR 0.0) 3432.11))  (setq dm172 (polar dm172t (DTR 90.0) 3522.85))  ;K172
				(setq dm173t (polar ibp (DTR 0.0) 3535.5))  (setq dm173 (polar dm173t (DTR 90.0) 3292.52))  ;K173
				(setq dm174t (polar ibp (DTR 0.0) 6592.52))  (setq dm174 (polar dm174t (DTR 90.0) 6140))  ;K174			
								
				(command "dim1" "ver" dm1 dm2 dm3 (fix expole_pedestalhight))
				(command "dim1" "ver" dm3 dm4 dm6 (fix joint_1))
				(command "dim1" "ver" dm4 dm5 dm6 (fix (- joint_0 joint_1)))
				(if(< newst1_newmem1_y1 newst1_newmem2_y1) 
					(progn 
						(command "dim1" "ver" dm6 dm7 dm11 (fix newst1_newmem1_y1))
						(command "dim1" "ver" dm7 dm8 dm11 (- newst1_newmem2_y1 newst1_newmem1_y1))
						(command "dim1" "ver" dm8 dm9 dm11 (fix (- newstud1_y1 newst1_newmem2_y1)))
						(command "dim1" "ver" dm9 dm10 dm11 (fix (- joint_0 newstud1_y1)))
						(command "_insert" "f2" dm25 "0.0394" "0.0394" "" "N4")
						(command "_insert" "f2" dm26 "0.0394" "0.0394" "" "N6")
					)
				)
				(if(< newst1_newmem2_y1 newst1_newmem1_y1) 
					(progn 
						(command "dim1" "ver" dm6 dm7 dm11 (fix newst1_newmem2_y1))
						(command "dim1" "ver" dm7 dm8 dm11 (- newst1_newmem1_y1 newst2_newmem1_y1))
						(command "dim1" "ver" dm8 dm9 dm11 (fix (- newstud1_y1 newst1_newmem1_y1)))
						(command "dim1" "ver" dm9 dm10 dm11 (fix (- joint_0 newstud1_y1)))
						(command "_insert" "f2" dm26 "0.0394" "0.0394" "" "N4")
						(command "_insert" "f2" dm25 "0.0394" "0.0394" "" "N6")
					)
				)
								
				(command "dim1" "ver" dm11 dm12 (polar dm12 (DTR 180.0) 130) (fix joint_0))
				(command "dim1" "hor" dm13 dm14 (polar dm14 (DTR 270.0) 140) (fix existingstud_x))
				(command "dim1" "hor" dm14 dm15 (polar dm14 (DTR 270.0) 140) (fix (- newstud1_x1 existingstud_x)))
				
				(command "style" "TRB" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" dm16 "0" (strcat "%%c" (rtos diameter_1 2 1 ) "x" (rtos diameter_1tick 2 1 ) "Thk"))
				(command "TEXT" dm17 "0" (strcat "%%c" (rtos diameter_2 2 1 ) "x" (rtos diameter_2tick 2 1 ) "Thk"))
				(command "TEXT" dm18 "0" exst1_name)
				(command "TEXT" dm19 "0" (strcat (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdwt 2 0 ) "x" (rtos newstud1_pdht 2 0 ) ))
				(command "TEXT" dm20 "0" (strcat "(POLE-" (itoa polenumber) ")"))
				(command "dim1" "hor" dm21 dm22 (polar dm22 (DTR 270.0) 167) (fix existingstud_x))
				(command "dim1" "hor" dm23 dm24 (polar dm23 (DTR 270.0) 170) (fix newstud1_x1))
				(command "dim1" "hor" dm27 dm28 (polar dm27 (DTR 90.0) 80.5) (fix newstud1_clampx1))
				(command "dim1" "hor" dm29 dm30 (polar dm30 (DTR 270.0) 81) (fix newstud1_clmapx2))
				(command "_insert" "f5x" dm31 "0.0394" "0.0394" "" "L11-1" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newstud1_clmapx2 2 0)))
				(command "dim1" "hor" dm32 dm33 (polar dm32 (DTR 90.0) 101) (fix newstud1_clampx1))
				(command "dim1" "hor" dm34 dm35 (polar dm35 (DTR 270.0) 78) (fix newstud1_clmapx2))
				(command "_insert" "f5x" dm36 "0.0394" "0.0394" "" "L11-2" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newstud1_clmapx2 2 0)))
				(command "dim1" "hor" dm37 dm38 (polar dm37 (DTR 90.0) 57) newstud1_platex2)
				(command "dim1" "hor" dm38 dm39 (polar dm39 (DTR 90.0) 57) newstud1_platex1)
				(command "dim1" "ver" dm37 dm40 (polar dm40 (DTR 180.0) 89) newstud1_platey2)
				(command "dim1" "ver" dm40 dm41 (polar dm40 (DTR 180.0) 89) newstud1_platey1)
				(command "_insert" "f5x" dm42 "0.0394" "0.0394" "" "L11-3" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x100x150" ))
				(command "dim1" "ali" dm43 dm44 "@" R_cc2 )
				(command "dim1" "ver" dm45 dm46 (polar dm45 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "ali" dm47 dm48 "@" R_cc2 )
				(command "dim1" "ver" dm49 dm50 (polar dm50 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "hor" dm51 dm52 (polar dm51 (DTR 90.0) 80.5) (fix newst1_newmem1_pclampx1))
				(command "dim1" "hor" dm53 dm54 (polar dm53 (DTR 270.0) 81) (fix newst1_newmem1_pclampx2))
				(command "_insert" "f5x" dm55 "0.0394" "0.0394" "" "L12-1" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newst1_newmem1_pclampx2 2 0)))
				(command "dim1" "hor" dm56 dm57 (polar dm56 (DTR 90.0) 101) (fix newst1_newmem1_pclampx1))
				(command "dim1" "hor" dm58 dm59 (polar dm58 (DTR 270.0) 78) (fix newst1_newmem1_pclampx2))
				(command "_insert" "f5x" dm60 "0.0394" "0.0394" "" "L12-2" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newst1_newmem1_pclampx2 2 0)))
				(command "_insert" "f5x" dm61 "0.0394" "0.0394" "" "L12-3" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x100x100" ))
				(command "dim1" "ali" dm62 dm63 "@" R_cc1 )
				(command "dim1" "ver" dm64 dm65 (polar dm64 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "ali" dm66 dm67 "@" R_cc1 )
				(command "dim1" "ver" dm68 dm69 (polar dm68 (DTR 0.0) 55) (rtos clampstk 2 0))
				(command "dim1" "ver" dm71 dm70 (polar dm71 (DTR 180.0) 80) (rtos newstud1_backmark 2 1))
				(command "dim1" "ver" dm71 dm72 (polar dm71 (DTR 180.0) 80) (rtos newstud1_backmark 2 1))
				(command "dim1" "hor" dm73 dm74 (polar dm73 (DTR 90.0) 65) newst1_sizename2)
				(command "dim1" "hor" dm75 dm76 (polar dm75 (DTR 90.0) 85) newst1_sizename2)
				(command "dim1" "hor" dm77 dm78 (polar dm78 (DTR 270.0) 157) (+ newst1_sizename2 35))
				(command "dim1" "hor" dm78 dm79 (polar dm79 (DTR 270.0) 157) (- newst1_joint3 (+ newst1_sizename2 35) 40))
				(command "dim1" "hor" dm79 dm80 (polar dm79 (DTR 270.0) 157) newst1_joint2)
				(command "dim1" "hor" dm80 dm81 (polar dm80 (DTR 270.0) 157) (- newst1_joint1 (+ newst1_sizename2 35) 40))
				(command "dim1" "hor" dm81 dm82 (polar dm81 (DTR 270.0) 157) (+ newst1_sizename2 35))
				(command "dim1" "ver" dm83 dm84 "@" newst1_sizename1)
				(command "_insert" "f3x" dm85 "0.0394" "0.0394" "" "N3" (strcat newstud1_memname ".." (itoa (Number_Round (+ newstud1_memlen 50) 1)) " Lg"))
				
				(command "dim1" "ver" dm87 dm86 (polar dm87 (DTR 180.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "ver" dm87 dm88 (polar dm87 (DTR 180.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "hor" dm89 dm90 (polar dm89 (DTR 90.0) 61) newst1_newmem1_sizename2)
				(command "dim1" "hor" dm91 dm92 (polar dm92 (DTR 90.0) 61) newst1_newmem1_sizename2)
				(command "dim1" "hor" dm93 dm94 (polar dm94 (DTR 270.0) 181) (+ newst1_newmem1_sizename2 35))
				(command "dim1" "hor" dm94 dm95 (polar dm95 (DTR 270.0) 181) (Number_Round (- newst1_newmem1_len (+ newst1_newmem1_sizename2 35) (+ newst1_newmem1_sizename2 35)) 1))
				(command "dim1" "hor" dm95 dm96 (polar dm95 (DTR 270.0) 181) (+ newst1_newmem1_sizename2 35))
				(command "dim1" "ver" dm97 dm98 "@" newst1_newmem1_sizename1)
				(command "_insert" "f3x" dm99 "0.0394" "0.0394" "" "N4" (strcat newst1_newmem1_memname ".." (itoa (Number_Round (+ newst1_newmem1_len 50) 1)) " Lg"))
				
				(command "dim1" "ver" dm101 dm100 (polar dm101 (DTR 180.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "ver" dm101 dm102 (polar dm101 (DTR 180.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "hor" dm103 dm104 (polar dm103 (DTR 90.0) 61) newst1_newmem2_sizename2)
				(command "dim1" "hor" dm105 dm106 (polar dm106 (DTR 90.0) 61) newst1_newmem2_sizename2)
				(command "dim1" "hor" dm107 dm108 (polar dm108 (DTR 270.0) 181) (+ newst1_newmem2_sizename2 35))
				(command "dim1" "hor" dm108 dm109 (polar dm109 (DTR 270.0) 181) (Number_Round (- newst1_newmem2_len (+ newst1_newmem2_sizename2 35) (+ newst1_newmem2_sizename2 35)) 1))
				(command "dim1" "hor" dm109 dm110 (polar dm109 (DTR 270.0) 181) (+ newst1_newmem2_sizename2 35))
				(command "dim1" "ver" dm111 dm112 "@" newst1_newmem2_sizename1)
				(command "_insert" "f3x" dm113 "0.0394" "0.0394" "" "N6" (strcat newst1_newmem2_memname ".." (itoa (Number_Round (+ newst1_newmem2_len 50) 1)) " Lg"))
				
				(command "dim1" "ver" dm115 dm114 (polar dm115 (DTR 180.0) 80)  (rtos newstud1_hipmem1_backmark 2 1))
				(command "dim1" "ver" dm115 dm116 (polar dm115 (DTR 180.0) 80)  (rtos newstud1_hipmem1_backmark 2 1))
				(command "dim1" "hor" dm117 dm118 (polar dm117 (DTR 90.0) 61) newstud1_hipmem1_sizename2)
				(command "dim1" "hor" dm119 dm120 (polar dm120 (DTR 90.0) 61) newstud1_hipmem1_sizename2)
				(command "dim1" "hor" dm121 dm122 (polar dm122 (DTR 270.0) 181) (+ newstud1_hipmem1_sizename2 35))
				(command "dim1" "hor" dm122 dm123 (polar dm123 (DTR 270.0) 181) (Number_Round (- newstud1_hipmem1_len (+ newstud1_hipmem1_sizename2 35) (+ newstud1_hipmem1_sizename2 35)) 1))
				(command "dim1" "hor" dm123 dm124 (polar dm123 (DTR 270.0) 181) (+ newstud1_hipmem1_sizename2 35))
				(command "dim1" "ver" dm125 dm126 "@" newstud1_hipmem1_sizename1)
				(command "_insert" "f3x" dm127 "0.0394" "0.0394" "" "N8" (strcat newst1_newmem2_memname ".." (itoa (Number_Round (+ newstud1_hipmem1_len 50) 1)) " Lg"))
				
				(command "dim1" "ver" dm129 dm128 (polar dm129 (DTR 180.0) 80)  (rtos newstud1_hipmem1_backmark 2 1))
				(command "dim1" "ver" dm129 dm130 (polar dm129 (DTR 180.0) 80)  (rtos newstud1_hipmem1_backmark 2 1))
				(command "dim1" "hor" dm131 dm132 (polar dm131 (DTR 90.0) 61) newstud1_hipmem1_sizename2)
				(command "dim1" "hor" dm133 dm134 (polar dm133 (DTR 90.0) 61) newstud1_hipmem1_sizename2)
				(command "dim1" "hor" dm135 dm136 (polar dm136 (DTR 270.0) 181) (+ newstud1_hipmem1_sizename2 35))
				(command "dim1" "hor" dm136 dm137 (polar dm136 (DTR 270.0) 181) (Number_Round (- newstud1_hipmem2_len (+ newstud1_hipmem1_sizename2 35) (+ newstud1_hipmem1_sizename2 35)) 1))
				(command "dim1" "hor" dm137 dm138 (polar dm123 (DTR 270.0) 181) (+ newstud1_hipmem1_sizename2 35))
				(command "dim1" "ver" dm139 dm140 "@" newstud1_hipmem2_sizename1)
				(command "_insert" "f3x" dm141 "0.0394" "0.0394" "" "N9" (strcat newst1_newmem2_memname ".." (itoa (Number_Round (+ newstud1_hipmem2_len 50) 1)) " Lg"))
				
				(command "dim1" "hor" dm142 dm143 (polar dm142 (DTR 90.0) 71) newst1_newmem1_stx2)
				(command "dim1" "hor" dm144 dm145 (polar dm144 (DTR 270.0) 90) (Number_Round newst1_newmem1_stx1 1))
				(command "_insert" "f3x" dm146 "0.0394" "0.0394" "" "N5" (strcat "PLT" (itoa (fix clampstk)) "x50.." (itoa (Number_Round newst1_newmem1_stx1 1)) " Lg"))
				(command "_insert" "f3x" dm147 "0.0394" "0.0394" "" "N7" (strcat "PLT" (itoa (fix clampstk)) "x50x100 Lg"))
				
				(command "dim1" "hor" dm148 dm149 (polar dm149 (DTR 90.0) 61) newst1_newmem2_sizename2)				
				(command "dim1" "ver" dm151 dm150 (polar dm151 (DTR 0.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "ver" dm151 dm152 (polar dm151 (DTR 0.0) 80) (rtos newst1_newmem2_backmark 2 1))
				(command "dim1" "hor" dm153 dm154 (polar dm154 (DTR 270.0) 115) (+ newst1_newmem2_sizename2 35))
				(command "dim1" "ver" dm155 dm156 "@" newst1_newmem2_sizename1)
				
				(command "dim1" "hor" dm157 dm158 (polar dm158 (DTR 90.0) 92) newst1_newmem1_sizename2)
				(command "dim1" "ver" dm160 dm159 (polar dm160 (DTR 0.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "ver" dm160 dm161 (polar dm160 (DTR 0.0) 80) (rtos newst1_newmem1_backmark 2 1))
				(command "dim1" "hor" dm162 dm163 (polar dm162 (DTR 270.0) 64) (+ newst1_newmem1_sizename2 35))
				(command "dim1" "ver" dm164 dm165 "@" newst1_newmem1_sizename1)
				
				(command "_insert" "f3x" dm166 "0.0394" "0.0394" "" "N1" (strcat "PL" (rtos bptk 2 0) "x230..230 Lg"))
				
				(command "dim1" "hor" dm167 dm168 (polar dm168 (DTR 90.0) 79) newstud1_platex1)
				(command "dim1" "hor" dm168 dm169 (polar dm168 (DTR 90.0) 79) newstud1_platex2)
				(command "dim1" "ver" dm170 dm171 (polar dm171 (DTR 180.0) 79) newstud1_platey1)
				(command "dim1" "ver" dm171 dm172 (polar dm172 (DTR 180.0) 79) newstud1_platey2)
				(command "_insert" "f3x" dm173 "0.0394" "0.0394" "" "N2" "L150x150x12..150 Lg")
				
				;BOM
				(command "_insert" "c:/detail/bolckss/IN-7.5M-RIT-SYAL&ASS-MP_P_SOL5_boq" dm174 "" "" "" )
				(setq N1 (* (/ bptk 1000.0 )(/ 230 1000.0) (/ 230 1000.0) 7850.0 3))
				(setq N2 (* (/ 150 1000.0) 22.9 3))
				(setq N3 (* (/ (Number_Round (+ newstud1_memlen 50) 1) 1000.0) newstud1_blockwt 3))
				(setq N4 (* (/ (Number_Round (+ newst1_newmem1_len 50) 1) 1000.0) newst1_newmem1_blockwt 3))
				(setq N5 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ (Number_Round newst1_newmem1_stx1 1) 1000.0) 7850.0 4))
				(setq N6 (* (/ (Number_Round (+ newst1_newmem2_len 50) 1) 1000.0) newst1_newmem2_blockwt 3))
				(setq N7 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 50 1000.0) 7850.0 4))
				(setq N8 (* (/ (Number_Round (+ newstud1_hipmem1_len 50) 1) 1000.0) newst1_newmem1_blockwt 3))
				(setq N9 (* (/ (Number_Round (+ newstud1_hipmem2_len 50) 1) 1000.0) newst1_newmem2_blockwt 3))
				(setq L11_1 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newstud1_clmapx2 1000.0) 7850.0))
				(setq L11_2 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newstud1_clmapx2 1000.0) 7850.0))
				(setq L11_3 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 150 1000.0) 7850.0 3))
				(setq L12_1 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newst1_newmem1_pclampx2 1000.0) 7850.0 2))
				(setq L12_2 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newst1_newmem1_pclampx2 1000.0) 7850.0 2))
				(setq L12_3 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 100 1000.0) 7850.0 6))
				(setq total_weight1 (+ N1 N2 N3 N4 N5 N6 N7 N8 N9 L11_1 L11_2 L11_3 L12_1 L12_2 L12_3))
				(setq concrete_vol (* (/ newstud1_pdwt 1000.0) (/ newstud1_pdwt 1000.0) (/ newstud1_pdht 1000.0) 2))
				(command "style" "TRB" "Trebuchet MS" (* 2.0 sf) "1" "" "" "")
				
				(setq bom1t (polar dm174 (DTR 180.0) 1991.67))  (setq bom1 (polar bom1t (DTR 270.0) 169.52))  ;jj1
				(setq bom2t (polar dm174 (DTR 180.0) 1625.47))  (setq bom2 (polar bom2t (DTR 270.0) 169.52))  ;jj2
				(setq bom3t (polar dm174 (DTR 180.0) 1286.32))  (setq bom3 (polar bom3t (DTR 270.0) 169.52))  ;jj3
				(setq bom4t (polar dm174 (DTR 180.0) 890.6))  (setq bom4 (polar bom4t (DTR 270.0) 167.91))  ;jj4
				(setq bom5t (polar dm174 (DTR 180.0) 531.92))  (setq bom5 (polar bom5t (DTR 270.0) 167.91))  ;jj5
				(setq bom6t (polar dm174 (DTR 180.0) 1991.67))  (setq bom6 (polar bom6t (DTR 270.0) 230.75))  ;jj6
				(setq bom7t (polar dm174 (DTR 180.0) 1625.47))  (setq bom7 (polar bom7t (DTR 270.0) 230.75))  ;jj7
				(setq bom8t (polar dm174 (DTR 180.0) 1286.32))  (setq bom8 (polar bom8t (DTR 270.0) 230.75))  ;jj8
				(setq bom9t (polar dm174 (DTR 180.0) 890.6))  (setq bom9 (polar bom9t (DTR 270.0) 231.33))  ;jj9
				(setq bom10t (polar dm174 (DTR 180.0) 531.92))  (setq bom10 (polar bom10t (DTR 270.0) 231.33))  ;jj10
				(setq bom11t (polar dm174 (DTR 180.0) 1991.67))  (setq bom11 (polar bom11t (DTR 270.0) 292.8))  ;jj11
				(setq bom12t (polar dm174 (DTR 180.0) 1625.47))  (setq bom12 (polar bom12t (DTR 270.0) 292.8))  ;jj12
				(setq bom13t (polar dm174 (DTR 180.0) 1286.32))  (setq bom13 (polar bom13t (DTR 270.0) 292.8))  ;jj13
				(setq bom14t (polar dm174 (DTR 180.0) 890.6))  (setq bom14 (polar bom14t (DTR 270.0) 293.49))  ;jj14
				(setq bom15t (polar dm174 (DTR 180.0) 531.92))  (setq bom15 (polar bom15t (DTR 270.0) 293.49))  ;jj15
				(setq bom16t (polar dm174 (DTR 180.0) 1991.67))  (setq bom16 (polar bom16t (DTR 270.0) 350.79))  ;jj16
				(setq bom17t (polar dm174 (DTR 180.0) 1625.47))  (setq bom17 (polar bom17t (DTR 270.0) 350.79))  ;jj17
				(setq bom18t (polar dm174 (DTR 180.0) 1286.32))  (setq bom18 (polar bom18t (DTR 270.0) 350.79))  ;jj18
				(setq bom19t (polar dm174 (DTR 180.0) 890.6))  (setq bom19 (polar bom19t (DTR 270.0) 349.19))  ;jj19
				(setq bom20t (polar dm174 (DTR 180.0) 531.92))  (setq bom20 (polar bom20t (DTR 270.0) 349.19))  ;jj20
				(setq bom21t (polar dm174 (DTR 180.0) 1991.67))  (setq bom21 (polar bom21t (DTR 270.0) 412.03))  ;jj21
				(setq bom22t (polar dm174 (DTR 180.0) 1625.47))  (setq bom22 (polar bom22t (DTR 270.0) 412.03))  ;jj22
				(setq bom23t (polar dm174 (DTR 180.0) 1286.32))  (setq bom23 (polar bom23t (DTR 270.0) 412.03))  ;jj23
				(setq bom24t (polar dm174 (DTR 180.0) 890.6))  (setq bom24 (polar bom24t (DTR 270.0) 412.61))  ;jj24
				(setq bom25t (polar dm174 (DTR 180.0) 531.92))  (setq bom25 (polar bom25t (DTR 270.0) 412.61))  ;jj25
				(setq bom26t (polar dm174 (DTR 180.0) 1991.67))  (setq bom26 (polar bom26t (DTR 270.0) 474.07))  ;jj26
				(setq bom27t (polar dm174 (DTR 180.0) 1625.47))  (setq bom27 (polar bom27t (DTR 270.0) 474.07))  ;jj27
				(setq bom28t (polar dm174 (DTR 180.0) 1286.32))  (setq bom28 (polar bom28t (DTR 270.0) 474.07))  ;jj28
				(setq bom29t (polar dm174 (DTR 180.0) 890.6))  (setq bom29 (polar bom29t (DTR 270.0) 474.77))  ;jj29
				(setq bom30t (polar dm174 (DTR 180.0) 531.92))  (setq bom30 (polar bom30t (DTR 270.0) 474.77))  ;jj30
				(setq bom31t (polar dm174 (DTR 180.0) 1967.75))  (setq bom31 (polar bom31t (DTR 270.0) 534.05))  ;jj31
				(setq bom32t (polar dm174 (DTR 180.0) 1625.47))  (setq bom32 (polar bom32t (DTR 270.0) 534.05))  ;jj32
				(setq bom33t (polar dm174 (DTR 180.0) 1286.32))  (setq bom33 (polar bom33t (DTR 270.0) 534.05))  ;jj33
				(setq bom34t (polar dm174 (DTR 180.0) 892.38))  (setq bom34 (polar bom34t (DTR 270.0) 535.26))  ;jj34
				(setq bom35t (polar dm174 (DTR 180.0) 546.05))  (setq bom35 (polar bom35t (DTR 270.0) 535.26))  ;jj35
				(setq bom36t (polar dm174 (DTR 180.0) 1967.75))  (setq bom36 (polar bom36t (DTR 270.0) 591.68))  ;jj36
				(setq bom37t (polar dm174 (DTR 180.0) 1625.47))  (setq bom37 (polar bom37t (DTR 270.0) 591.68))  ;jj37
				(setq bom38t (polar dm174 (DTR 180.0) 1286.32))  (setq bom38 (polar bom38t (DTR 270.0) 591.68))  ;jj38
				(setq bom39t (polar dm174 (DTR 180.0) 902.12))  (setq bom39 (polar bom39t (DTR 270.0) 591.68))  ;jj39
				(setq bom40t (polar dm174 (DTR 180.0) 555.79))  (setq bom40 (polar bom40t (DTR 270.0) 592.9))  ;jj40
				(setq bom41t (polar dm174 (DTR 180.0) 1967.75))  (setq bom41 (polar bom41t (DTR 270.0) 650.48))  ;jj41
				(setq bom42t (polar dm174 (DTR 180.0) 1625.47))  (setq bom42 (polar bom42t (DTR 270.0) 650.48))  ;jj42
				(setq bom43t (polar dm174 (DTR 180.0) 1286.32))  (setq bom43 (polar bom43t (DTR 270.0) 650.48))  ;jj43
				(setq bom44t (polar dm174 (DTR 180.0) 911.85))  (setq bom44 (polar bom44t (DTR 270.0) 651.7))  ;jj44
				(setq bom45t (polar dm174 (DTR 180.0) 565.52))  (setq bom45 (polar bom45t (DTR 270.0) 651.7))  ;jj45
				(setq bom46t (polar dm174 (DTR 180.0) 1967.75))  (setq bom46 (polar bom46t (DTR 270.0) 708.48))  ;jj46
				(setq bom47t (polar dm174 (DTR 180.0) 1625.47))  (setq bom47 (polar bom47t (DTR 270.0) 708.48))  ;jj47
				(setq bom48t (polar dm174 (DTR 180.0) 1286.32))  (setq bom48 (polar bom48t (DTR 270.0) 708.48))  ;jj48
				(setq bom49t (polar dm174 (DTR 180.0) 892.38))  (setq bom49 (polar bom49t (DTR 270.0) 711.72))  ;jj49
				(setq bom50t (polar dm174 (DTR 180.0) 546.05))  (setq bom50 (polar bom50t (DTR 270.0) 711.72))  ;jj50
				(setq bom51t (polar dm174 (DTR 180.0) 1967.75))  (setq bom51 (polar bom51t (DTR 270.0) 770.52))  ;jj51
				(setq bom52t (polar dm174 (DTR 180.0) 1625.47))  (setq bom52 (polar bom52t (DTR 270.0) 770.52))  ;jj52
				(setq bom53t (polar dm174 (DTR 180.0) 1286.32))  (setq bom53 (polar bom53t (DTR 270.0) 770.52))  ;jj53
				(setq bom54t (polar dm174 (DTR 180.0) 892.38))  (setq bom54 (polar bom54t (DTR 270.0) 771.74))  ;jj54
				(setq bom55t (polar dm174 (DTR 180.0) 546.05))  (setq bom55 (polar bom55t (DTR 270.0) 771.74))  ;jj55
				(setq bom56t (polar dm174 (DTR 180.0) 1967.75))  (setq bom56 (polar bom56t (DTR 270.0) 828.51))  ;jj56
				(setq bom57t (polar dm174 (DTR 180.0) 1625.47))  (setq bom57 (polar bom57t (DTR 270.0) 828.51))  ;jj57
				(setq bom58t (polar dm174 (DTR 180.0) 1286.32))  (setq bom58 (polar bom58t (DTR 270.0) 828.51))  ;jj58
				(setq bom59t (polar dm174 (DTR 180.0) 902.12))  (setq bom59 (polar bom59t (DTR 270.0) 831.76))  ;jj59
				(setq bom60t (polar dm174 (DTR 180.0) 555.79))  (setq bom60 (polar bom60t (DTR 270.0) 831.76))  ;jj60
				(setq bom61t (polar dm174 (DTR 180.0) 1967.75))  (setq bom61 (polar bom61t (DTR 270.0) 890.56))  ;jj61
				(setq bom62t (polar dm174 (DTR 180.0) 1625.47))  (setq bom62 (polar bom62t (DTR 270.0) 890.56))  ;jj62
				(setq bom63t (polar dm174 (DTR 180.0) 1286.32))  (setq bom63 (polar bom63t (DTR 270.0) 890.56))  ;jj63
				(setq bom64t (polar dm174 (DTR 180.0) 892.38))  (setq bom64 (polar bom64t (DTR 270.0) 891.78))  ;jj64
				(setq bom65t (polar dm174 (DTR 180.0) 546.05))  (setq bom65 (polar bom65t (DTR 270.0) 891.78))  ;jj65
				(setq bom66t (polar dm174 (DTR 180.0) 1967.75))  (setq bom66 (polar bom66t (DTR 270.0) 948.55))  ;jj66
				(setq bom67t (polar dm174 (DTR 180.0) 1625.47))  (setq bom67 (polar bom67t (DTR 270.0) 948.55))  ;jj67
				(setq bom68t (polar dm174 (DTR 180.0) 1286.32))  (setq bom68 (polar bom68t (DTR 270.0) 948.55))  ;jj68
				(setq bom69t (polar dm174 (DTR 180.0) 902.12))  (setq bom69 (polar bom69t (DTR 270.0) 951.8))  ;jj69
				(setq bom70t (polar dm174 (DTR 180.0) 555.79))  (setq bom70 (polar bom70t (DTR 270.0) 951.8))  ;jj70
				(setq bom71t (polar dm174 (DTR 180.0) 1967.75))  (setq bom71 (polar bom71t (DTR 270.0) 1007.36))  ;jj71
				(setq bom72t (polar dm174 (DTR 180.0) 1625.47))  (setq bom72 (polar bom72t (DTR 270.0) 1007.36))  ;jj72
				(setq bom73t (polar dm174 (DTR 180.0) 1294.43))  (setq bom73 (polar bom73t (DTR 270.0) 1007.36))  ;jj73
				(setq bom74t (polar dm174 (DTR 180.0) 902.12))  (setq bom74 (polar bom74t (DTR 270.0) 1010.6))  ;jj74
				(setq bom75t (polar dm174 (DTR 180.0) 555.79))  (setq bom75 (polar bom75t (DTR 270.0) 1010.6))  ;jj75
				(setq bom76t (polar dm174 (DTR 180.0) 923.61))  (setq bom76 (polar bom76t (DTR 270.0) 1069.4))  ;jj76
				(setq bom77t (polar dm174 (DTR 180.0) 577.28))  (setq bom77 (polar bom77t (DTR 270.0) 1069.4))  ;jj77
				(setq bom78t (polar dm174 (DTR 180.0) 934.97))  (setq bom78 (polar bom78t (DTR 270.0) 1476.16))  ;jj78
				(setq bom79t (polar dm174 (DTR 180.0) 588.64))  (setq bom79 (polar bom79t (DTR 270.0) 1476.16))  ;jj79
				(setq bom80t (polar dm174 (DTR 180.0) 934.97))  (setq bom80 (polar bom80t (DTR 270.0) 1603.5))  ;jj80
				(setq bom81t (polar dm174 (DTR 180.0) 588.64))  (setq bom81 (polar bom81t (DTR 270.0) 1603.5))  ;jj81
				(setq bom82t (polar dm174 (DTR 180.0) 672.15))  (setq bom82 (polar bom82t (DTR 270.0) 1663.67))  ;jj82
				
				(command "TEXT" bom1 "0" "L11-1")
				(command "TEXT" bom2 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom3 "0" (rtos newstud1_clmapx2 2 0))
				(command "TEXT" bom4 "0" (rtos L11_1 2 2))
				(command "TEXT" bom5 "0" (rtos (* 1.035 L11_1) 2 2))
				(command "TEXT" bom6 "0" "L11-2")
				(command "TEXT" bom7 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom8 "0" (rtos newstud1_clmapx2 2 0))
				(command "TEXT" bom9 "0" (rtos L11_2 2 2))
				(command "TEXT" bom10 "0" (rtos (* 1.035 L11_2) 2 2))
				(command "TEXT" bom11 "0" "L11-3")
				(command "TEXT" bom12 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x100" ))
				(command "TEXT" bom13 "0" "150")
				(command "TEXT" bom14 "0" (rtos L11_3 2 2))
				(command "TEXT" bom15 "0" (rtos (* 1.035 L11_3 ) 2 2))
				
				(command "TEXT" bom16 "0" "L12-1")
				(command "TEXT" bom17 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom18 "0" (rtos newst1_newmem1_pclampx2 2 0))
				(command "TEXT" bom19 "0" (rtos L12_1 2 2))
				(command "TEXT" bom20 "0" (rtos (* 1.035 L12_1) 2 2))
				(command "TEXT" bom21 "0" "L12-2")
				(command "TEXT" bom22 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x130" ))
				(command "TEXT" bom23 "0" (rtos newst1_newmem1_pclampx2 2 0))
				(command "TEXT" bom24 "0" (rtos L12_2 2 2))
				(command "TEXT" bom25 "0" (rtos (* 1.035 L12_2) 2 2))
				(command "TEXT" bom26 "0" "L12-3")
				(command "TEXT" bom27 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x100" ))
				(command "TEXT" bom28 "0" "100")
				(command "TEXT" bom29 "0" (rtos L12_3 2 2))
				(command "TEXT" bom30 "0" (rtos (* 1.035 L12_3) 2 2))
				
				(command "TEXT" bom31 "0" "N1")
				(command "TEXT" bom32 "0" (strcat "PL" (rtos bptk 2 0) "x230"))
				(command "TEXT" bom33 "0" "230")
				(command "TEXT" bom34 "0" (rtos N1 2 2))
				(command "TEXT" bom35 "0" (rtos (* 1.035 N1) 2 2))
				(command "TEXT" bom36 "0" "N2")
				(command "TEXT" bom37 "0" "L150x10")
				(command "TEXT" bom38 "0" "150")
				(command "TEXT" bom39 "0" (rtos N2 2 2))
				(command "TEXT" bom40 "0" (rtos (* 1.035 N2) 2 2))
				(command "TEXT" bom41 "0" "N3")
				(command "TEXT" bom42 "0" newstud1_memname)
				(command "TEXT" bom43 "0" (itoa (Number_Round (+ newstud1_memlen 50) 1)))
				(command "TEXT" bom44 "0" (rtos N3 2 2))
				(command "TEXT" bom45 "0" (rtos (* 1.035 N3 ) 2 2))
				(command "TEXT" bom46 "0" "N4")
				(command "TEXT" bom47 "0" newst1_newmem1_memname)
				(command "TEXT" bom48 "0" (itoa (Number_Round (+ newst1_newmem1_len 50) 1)))
				(command "TEXT" bom49 "0" (rtos N4 2 2))
				(command "TEXT" bom50 "0" (rtos (* 1.035 N4 ) 2 2))
				
				(command "TEXT" bom51 "0" "N5")
				(command "TEXT" bom52 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x100"))
				(command "TEXT" bom53 "0" (itoa (Number_Round newst1_newmem1_stx1 1)))
				(command "TEXT" bom54 "0" (rtos N5 2 2))
				(command "TEXT" bom55 "0" (rtos (* 1.035 N5 ) 2 2))
				(command "TEXT" bom56 "0" "N6")
				(command "TEXT" bom57 "0" newst1_newmem2_memname)
				(command "TEXT" bom58 "0" (itoa (Number_Round (+ newst1_newmem2_len 50) 1)))
				(command "TEXT" bom59 "0" (rtos N6 2 2))
				(command "TEXT" bom60 "0" (rtos (* 1.035 N6 ) 2 2))
				
				(command "TEXT" bom61 "0" "N7")
				(command "TEXT" bom62 "0" (strcat "PL" (rtos (fix clampstk) 2 0) "x50"))
				(command "TEXT" bom63 "0" "100")
				(command "TEXT" bom64 "0" (rtos N7 2 2))
				(command "TEXT" bom65 "0" (rtos (* 1.035 N7 ) 2 2))
				
				(command "TEXT" bom66 "0" "N8")
				(command "TEXT" bom67 "0" newstud1_hipmem1_memname)
				(command "TEXT" bom68 "0" (itoa (Number_Round (+ newstud1_hipmem1_len 50) 1)))
				(command "TEXT" bom69 "0" (rtos N8 2 2))
				(command "TEXT" bom70 "0" (rtos (* 1.035 N8) 2 2))
				
				(command "TEXT" bom71 "0" "N9")
				(command "TEXT" bom72 "0" newstud1_hipmem1_memname)
				(command "TEXT" bom73 "0" (itoa (Number_Round (+ newstud1_hipmem2_len 50) 1)))
				(command "TEXT" bom74 "0" (rtos N9 2 2))
				(command "TEXT" bom75 "0" (rtos (* 1.035 N9) 2 2))
				
				(command "TEXT" bom76 "0" (rtos total_weight1 2 2))
				(command "TEXT" bom77 "0" (rtos (* total_weight1 1.035) 2 2))
				(command "TEXT" bom82 "0" (rtos concrete_vol 2 3))
				(command "style" "WMF-Trebuchet MS0" "Trebuchet MS" (* 2.5 sf) "1" "" "" "")
				(command "TEXT" bom78 "0" (rtos total_weight1 2 2))
				(command "TEXT" bom79 "0" (rtos (* total_weight1 1.035) 2 2))
				(command "TEXT" bom80 "0" (rtos (+ total_weight1 6.36) 2 2))
				(command "TEXT" bom81 "0" (rtos (* (+ total_weight1 6.36) 1.035) 2 2))
				
				(setq next1 (polar ibp (dtr 0.0) 9900))
				(setq ibp next1)

		))
	
	))
))
(if (= designtype "UKPD") (progn
(if (and (= designtype "UKPD")(= existingstud 2) (= existingstud_membertype "A") (= newstudnum 1) (= newstud1_memty "P") (= newst_mem_onpole 1) (= newst_mem_onstruct 1) (= newst1_newmem1_type "P")) 
(progn
(if (or (> newstud1_memlen 6000) (> newstud1_memlen 6000)) (progn
(if (= ibp nil) (setq ibp '(0.0 0.0 0.0)))
(command "_insert" "c:/detail/bolckss/ukp_2pj_p_p_j1_1" ibp "" "" "" )
(if (= polenumber nil) (setq polenumber 0) (setq polenumber (1+ polenumber)))

(setq dm1 (polar ibp (DTR 0.0) 958.336))  (setq dm2 (polar dm1 (DTR 90.0) 1805.11))  ;P1
(setq dm3 (polar ibp (DTR 0.0) 1055.83))  (setq dm4 (polar dm3 (DTR 90.0) 3555.11))  ;P2
(setq dm5 (polar ibp (DTR 0.0) 1055.83))  (setq dm6 (polar dm5 (DTR 90.0) 5055.11))  ;P3
(setq dm7 (polar ibp (DTR 0.0) 1055.83))  (setq dm8 (polar dm7 (DTR 90.0) 6069.95))  ;P4
(setq dm9 (polar ibp (DTR 0.0) 668.119))  (setq dm10 (polar dm9 (DTR 90.0) 1805.12))  ;P5
(setq dm11 (polar ibp (DTR 0.0) 668.119))  (setq dm12 (polar dm11 (DTR 90.0) 3625.83))  ;P6
(setq dm13 (polar ibp (DTR 0.0) 668.119))  (setq dm14 (polar dm13 (DTR 90.0) 5155.12))  ;P7
(setq dm15 (polar ibp (DTR 0.0) 668.119))  (setq dm16 (polar dm15 (DTR 90.0) 6069.95))  ;P8
(setq dm17 (polar ibp (DTR 0.0) 563.124))  (setq dm18 (polar dm17 (DTR 90.0) 1805.11))  ;P9
(setq dm19 (polar ibp (DTR 0.0) 563.119))  (setq dm20 (polar dm19 (DTR 90.0) 6069.95))  ;P10
(setq dm21 (polar ibp (DTR 0.0) 780.10))  (setq dm22 (polar dm21 (DTR 90.0) 2248.86))  ;P11
(setq dm23 (polar ibp (DTR 0.0) 790.030))  (setq dm24 (polar dm23 (DTR 90.0) 3971.14))  ;P12
(setq dm25 (polar ibp (DTR 0.0) 780.710))  (setq dm26 (polar dm25 (DTR 90.0) 5440.47))  ;P13
(setq dm27 (polar ibp (DTR 0.0) 1234.21))  (setq dm28 (polar dm27 (DTR 90.0) 1392.99))  ;P14
(setq dm29 (polar ibp (DTR 0.0) 4035.18))  (setq dm30 (polar dm29 (DTR 90.0) 1665.85))  ;P15
(setq dm31 (polar ibp (DTR 0.0) 4501.68))  (setq dm32 (polar dm31 (DTR 90.0) 5916.46))  ;P16
(setq dm33 (polar ibp (DTR 0.0) 4501.68))  (setq dm34 (polar dm33 (DTR 90.0) 5836.46))  ;P17
(setq dm35 (polar ibp (DTR 0.0) 4501.68))  (setq dm36 (polar dm35 (DTR 90.0) 5756.46))  ;P18
(setq dm37 (polar ibp (DTR 0.0) 4670.72))  (setq dm38 (polar dm37 (DTR 90.0) 5756.46))  ;P19
(setq dm39 (polar ibp (DTR 0.0) 4910.72))  (setq dm40 (polar dm39 (DTR 90.0) 5776.16))  ;P20
(setq dm41 (polar ibp (DTR 0.0) 6272.03))  (setq dm42 (polar dm41 (DTR 90.0) 5767.44))  ;P21
(setq dm43 (polar ibp (DTR 0.0) 4910.72))  (setq dm44 (polar dm43 (DTR 90.0) 5896.76))  ;P22
(setq dm45 (polar ibp (DTR 0.0) 4730.72))  (setq dm46 (polar dm45 (DTR 90.0) 5916.46))  ;P23
(setq dm47 (polar ibp (DTR 0.0) 5120.66))  (setq dm48 (polar dm47 (DTR 90.0) 5896.76))  ;P24
(setq dm49 (polar ibp (DTR 0.0) 5122.15))  (setq dm50 (polar dm49 (DTR 90.0) 5776.16))  ;P25
(setq dm51 (polar ibp (DTR 0.0) 5094.56))  (setq dm52 (polar dm51 (DTR 90.0) 5534.36))  ;P26
(setq dm53 (polar ibp (DTR 0.0) 4510.16))  (setq dm54 (polar dm53 (DTR 90.0) 5249.64))  ;P27
(setq dm55 (polar ibp (DTR 0.0) 6056.51))  (setq dm56 (polar dm55 (DTR 90.0) 5264.30))  ;P28
(setq dm57 (polar ibp (DTR 0.0) 6296.51))  (setq dm58 (polar dm57 (DTR 90.0) 5244.60))  ;P29
(setq dm59 (polar ibp (DTR 0.0) 6465.55))  (setq dm60 (polar dm59 (DTR 90.0) 5244.60))  ;P30
(setq dm61 (polar ibp (DTR 0.0) 6465.55))  (setq dm62 (polar dm61 (DTR 90.0) 5324.60))  ;P31
(setq dm63 (polar ibp (DTR 0.0) 6465.55))  (setq dm64 (polar dm63 (DTR 90.0) 5404.60))  ;P32
(setq dm65 (polar ibp (DTR 0.0) 6236.51))  (setq dm66 (polar dm65 (DTR 90.0) 5404.60))  ;P33
(setq dm67 (polar ibp (DTR 0.0) 6056.51))  (setq dm68 (polar dm67 (DTR 90.0) 5384.94))  ;P34
(setq dm69 (polar ibp (DTR 0.0) 5062.42))  (setq dm70 (polar dm69 (DTR 90.0) 4984.83))  ;P35
(setq dm71 (polar ibp (DTR 0.0) 4670.38))  (setq dm72 (polar dm71 (DTR 90.0) 4769.87))  ;P36
(setq dm73 (polar ibp (DTR 0.0) 4670.38))  (setq dm74 (polar dm73 (DTR 90.0) 4700.87))  ;P37
(setq dm75 (polar ibp (DTR 0.0) 4670.38))  (setq dm76 (polar dm75 (DTR 90.0) 4631.87))  ;P38
(setq dm77 (polar ibp (DTR 0.0) 4720.38))  (setq dm78 (polar dm77 (DTR 90.0) 4631.87))  ;P39
(setq dm79 (polar ibp (DTR 0.0) 4950.38))  (setq dm80 (polar dm79 (DTR 90.0) 4652.57))  ;P40
(setq dm81 (polar ibp (DTR 0.0) 6076.43))  (setq dm82 (polar dm81 (DTR 90.0) 4652.57))  ;P41
(setq dm83 (polar ibp (DTR 0.0) 6306.43))  (setq dm84 (polar dm83 (DTR 90.0) 4631.87))  ;P42
(setq dm85 (polar ibp (DTR 0.0) 6356.43))  (setq dm86 (polar dm85 (DTR 90.0) 4631.87))  ;P43
(setq dm87 (polar ibp (DTR 0.0) 6356.43))  (setq dm88 (polar dm87 (DTR 90.0) 4700.87))  ;P44
(setq dm89 (polar ibp (DTR 0.0) 6356.43))  (setq dm90 (polar dm89 (DTR 90.0) 4769.87))  ;P45
(setq dm91 (polar ibp (DTR 0.0) 6226.43))  (setq dm92 (polar dm91 (DTR 90.0) 4769.87))  ;P46
(setq dm93 (polar ibp (DTR 0.0) 6076.43))  (setq dm94 (polar dm93 (DTR 90.0) 4749.17))  ;P47
(setq dm95 (polar ibp (DTR 0.0) 4950.38))  (setq dm96 (polar dm95 (DTR 90.0) 4749.17))  ;P48
(setq dm97 (polar ibp (DTR 0.0) 4800.38))  (setq dm98 (polar dm97 (DTR 90.0) 4769.87))  ;P49
(setq dm99 (polar ibp (DTR 0.0) 4940.09))  (setq dm100 (polar dm99 (DTR 90.0) 5264.34))  ;P50
(setq dm101 (polar ibp (DTR 0.0) 4944.10))  (setq dm102 (polar dm101 (DTR 90.0) 5384.94))  ;P51
(setq dm103 (polar ibp (DTR 0.0) 5195.30))  (setq dm104 (polar dm103 (DTR 90.0) 4652.57))  ;P52
(setq dm105 (polar ibp (DTR 0.0) 5201.30))  (setq dm106 (polar dm105 (DTR 90.0) 4749.17))  ;P53
(setq dm107 (polar ibp (DTR 0.0) 5138.55))  (setq dm108 (polar dm107 (DTR 90.0) 4340.70))  ;P54
(setq dm109 (polar ibp (DTR 0.0) 7080.48))  (setq dm110 (polar dm109 (DTR 90.0) 5406.26))  ;P55
(setq dm111 (polar ibp (DTR 0.0) 7305.48))  (setq dm112 (polar dm111 (DTR 90.0) 5437.31))  ;P56
(setq dm113 (polar ibp (DTR 0.0) 7580.74))  (setq dm114 (polar dm113 (DTR 90.0) 5437.31))  ;P57
(setq dm115 (polar ibp (DTR 0.0) 7580.74))  (setq dm116 (polar dm115 (DTR 90.0) 5333.81))  ;P58
(setq dm117 (polar ibp (DTR 0.0) 7580.74))  (setq dm118 (polar dm117 (DTR 90.0) 5230.31))  ;P59
(setq dm119 (polar ibp (DTR 0.0) 7425.48))  (setq dm120 (polar dm119 (DTR 90.0) 5230.31))  ;P60
(setq dm121 (polar ibp (DTR 0.0) 7305.48))  (setq dm122 (polar dm121 (DTR 90.0) 5230.31))  ;P61
(setq dm123 (polar ibp (DTR 0.0) 7080.48))  (setq dm124 (polar dm123 (DTR 90.0) 5261.36))  ;P62
(setq dm125 (polar ibp (DTR 0.0) 8106.43))  (setq dm126 (polar dm125 (DTR 90.0) 5406.26))  ;P63
(setq dm127 (polar ibp (DTR 0.0) 8331.43))  (setq dm128 (polar dm127 (DTR 90.0) 5437.31))  ;P64
(setq dm129 (polar ibp (DTR 0.0) 8526.43))  (setq dm130 (polar dm129 (DTR 90.0) 5437.31))  ;P65
(setq dm131 (polar ibp (DTR 0.0) 8526.43))  (setq dm132 (polar dm131 (DTR 90.0) 5333.81))  ;P66
(setq dm133 (polar ibp (DTR 0.0) 8526.43))  (setq dm134 (polar dm133 (DTR 90.0) 5230.31))  ;P67
(setq dm135 (polar ibp (DTR 0.0) 8451.43))  (setq dm136 (polar dm135 (DTR 90.0) 5230.31))  ;P68
(setq dm137 (polar ibp (DTR 0.0) 8106.43))  (setq dm138 (polar dm137 (DTR 90.0) 5261.36))  ;P69
(setq dm139 (polar ibp (DTR 0.0) 8353.43))  (setq dm140 (polar dm139 (DTR 90.0) 4770.64))  ;P70
(setq dm141 (polar ibp (DTR 0.0) 8476.10))  (setq dm142 (polar dm141 (DTR 90.0) 4770.64))  ;P71
(setq dm143 (polar ibp (DTR 0.0) 8073.37))  (setq dm144 (polar dm143 (DTR 90.0) 4570.64))  ;P72
(setq dm145 (polar ibp (DTR 0.0) 8476.10))  (setq dm146 (polar dm145 (DTR 90.0) 4570.64))  ;P73
(setq dm147 (polar ibp (DTR 0.0) 7938.97))  (setq dm148 (polar dm147 (DTR 90.0) 4338.06))  ;P74
(setq dm149 (polar ibp (DTR 0.0) 7805.27))  (setq dm150 (polar dm149 (DTR 90.0) 3347.63))  ;P75
(setq dm151 (polar ibp (DTR 0.0) 7773.75))  (setq dm152 (polar dm151 (DTR 90.0) 3100.54))  ;P76
(setq dm153 (polar ibp (DTR 0.0) 7882.05))  (setq dm154 (polar dm153 (DTR 90.0) 2859.34))  ;P77
(setq dm155 (polar ibp (DTR 0.0) 7925.74))  (setq dm156 (polar dm155 (DTR 90.0) 2410.25))  ;P78
(setq dm157 (polar ibp (DTR 0.0) 8038.22))  (setq dm158 (polar dm157 (DTR 90.0) 2410.53))  ;P79
(setq dm159 (polar ibp (DTR 0.0) 8172.94))  (setq dm160 (polar dm159 (DTR 90.0) 2410.53))  ;P80
(setq dm161 (polar ibp (DTR 0.0) 7842.94))  (setq dm162 (polar dm161 (DTR 90.0) 2327.73))  ;P81
(setq dm163 (polar ibp (DTR 0.0) 7842.94))  (setq dm164 (polar dm163 (DTR 90.0) 2215.25))  ;P82
(setq dm165 (polar ibp (DTR 0.0) 7842.94))  (setq dm166 (polar dm165 (DTR 90.0) 2065.25))  ;P83
(setq dm167 (polar ibp (DTR 0.0) 7920.88))  (setq dm168 (polar dm167 (DTR 90.0) 1865.17))  ;P84
(setq dm169 (polar ibp (DTR 0.0) 6807.02))  (setq dm170 (polar dm169 (DTR 90.0) 1735.35))  ;P85
(setq dm171 (polar ibp (DTR 0.0) 4604.34))  (setq dm172 (polar dm171 (DTR 90.0) 3427.39))  ;P86
(setq dm173 (polar ibp (DTR 0.0) 4877.47))  (setq dm174 (polar dm173 (DTR 90.0) 3427.39))  ;P87
(setq dm175 (polar ibp (DTR 0.0) 4488.15))  (setq dm176 (polar dm175 (DTR 90.0) 3223.56))  ;P88
(setq dm177 (polar ibp (DTR 0.0) 4993.66))  (setq dm178 (polar dm177 (DTR 90.0) 3223.56))  ;P89
(setq dm179 (polar ibp (DTR 0.0) 4446.54))  (setq dm180 (polar dm179 (DTR 90.0) 3024.86))  ;P90
(setq dm181 (polar ibp (DTR 0.0) 5346.82))  (setq dm182 (polar dm181 (DTR 90.0) 3411.89))  ;P91
(setq dm183 (polar ibp (DTR 0.0) 5624.40))  (setq dm184 (polar dm183 (DTR 90.0) 3411.89))  ;P92
(setq dm185 (polar ibp (DTR 0.0) 5228.75))  (setq dm186 (polar dm185 (DTR 90.0) 3204.75))  ;P93
(setq dm187 (polar ibp (DTR 0.0) 5742.48))  (setq dm188 (polar dm187 (DTR 90.0) 3204.75))  ;P94
(setq dm189 (polar ibp (DTR 0.0) 5180.32))  (setq dm190 (polar dm189 (DTR 90.0) 3019.09))  ;P95
(setq dm191 (polar ibp (DTR 0.0) 6103.26))  (setq dm192 (polar dm191 (DTR 90.0) 3303.69))  ;P96
(setq dm193 (polar ibp (DTR 0.0) 6103.86))  (setq dm194 (polar dm193 (DTR 90.0) 3394.48))  ;P97
(setq dm195 (polar ibp (DTR 0.0) 6103.26))  (setq dm196 (polar dm195 (DTR 90.0) 3494.48))  ;P98
(setq dm197 (polar ibp (DTR 0.0) 6214.46))  (setq dm198 (polar dm197 (DTR 90.0) 3494.48))  ;P99
(setq dm199 (polar ibp (DTR 0.0) 6332.31))  (setq dm200 (polar dm199 (DTR 90.0) 3494.48))  ;P100
(setq dm201 (polar ibp (DTR 0.0) 5875.70))  (setq dm202 (polar dm201 (DTR 90.0) 3056.62))  ;P101
(setq dm203 (polar ibp (DTR 0.0) 6063.51))  (setq dm204 (polar dm203 (DTR 90.0) 3819.79))  ;P102
(setq dm205 (polar ibp (DTR 0.0) 6137.66))  (setq dm206 (polar dm205 (DTR 90.0) 3866.11))  ;P103
(setq dm207 (polar ibp (DTR 0.0) 6376.27))  (setq dm208 (polar dm207 (DTR 90.0) 3843.03))  ;P104
(setq dm209 (polar ibp (DTR 0.0) 6376.27))  (setq dm210 (polar dm209 (DTR 90.0) 3858.41))  ;P105
(setq dm211 (polar ibp (DTR 0.0) 6030.79))  (setq dm212 (polar dm211 (DTR 90.0) 3681.00))  ;P106
(setq dm213 (polar ibp (DTR 0.0) 6244.53))  (setq dm214 (polar dm213 (DTR 90.0) 3681.00))  ;P107
(setq dm215 (polar ibp (DTR 0.0) 6069.13))  (setq dm216 (polar dm215 (DTR 90.0) 4051.53))  ;P108
(setq dm217 (polar ibp (DTR 0.0) 6137.66))  (setq dm218 (polar dm217 (DTR 90.0) 4105.82))  ;P109
(setq dm219 (polar ibp (DTR 0.0) 6376.27))  (setq dm220 (polar dm219 (DTR 90.0) 4082.74))  ;P110
(setq dm221 (polar ibp (DTR 0.0) 6376.27))  (setq dm222 (polar dm221 (DTR 90.0) 4098.13))  ;P111
(setq dm223 (polar ibp (DTR 0.0) 5700.01))  (setq dm224 (polar dm223 (DTR 90.0) 2887.93))  ;P112
(setq dm225 (polar ibp (DTR 0.0) 4592.68))  (setq dm226 (polar dm225 (DTR 90.0) 1957.07))  ;P113
(setq dm227 (polar ibp (DTR 0.0) 4865.82))  (setq dm228 (polar dm227 (DTR 90.0) 1957.07))  ;P114
(setq dm229 (polar ibp (DTR 0.0) 4476.50))  (setq dm230 (polar dm229 (DTR 90.0) 1753.23))  ;P115
(setq dm231 (polar ibp (DTR 0.0) 4982.00))  (setq dm232 (polar dm231 (DTR 90.0) 1753.23))  ;P116
(setq dm233 (polar ibp (DTR 0.0) 4434.89))  (setq dm234 (polar dm233 (DTR 90.0) 1554.53))  ;P117
(setq dm235 (polar ibp (DTR 0.0) 5348.45))  (setq dm236 (polar dm235 (DTR 90.0) 1941.57))  ;P118
(setq dm237 (polar ibp (DTR 0.0) 5626.03))  (setq dm238 (polar dm237 (DTR 90.0) 1941.57))  ;P119
(setq dm239 (polar ibp (DTR 0.0) 5230.38))  (setq dm240 (polar dm239 (DTR 90.0) 1734.42))  ;P120
(setq dm241 (polar ibp (DTR 0.0) 5744.10))  (setq dm242 (polar dm241 (DTR 90.0) 1734.42))  ;P121
(setq dm243 (polar ibp (DTR 0.0) 5168.84))  (setq dm244 (polar dm243 (DTR 90.0) 1548.76))  ;P122
(setq dm245 (polar ibp (DTR 0.0) 5864.05))  (setq dm246 (polar dm245 (DTR 90.0) 1586.29))  ;P123
(setq dm247 (polar ibp (DTR 0.0) 6048.08))  (setq dm248 (polar dm247 (DTR 90.0) 2389.02))  ;P124
(setq dm249 (polar ibp (DTR 0.0) 6125.18))  (setq dm250 (polar dm249 (DTR 90.0) 2430.26))  ;P125
(setq dm251 (polar ibp (DTR 0.0) 6363.78))  (setq dm252 (polar dm251 (DTR 90.0) 2407.18))  ;P126
(setq dm253 (polar ibp (DTR 0.0) 6363.78))  (setq dm254 (polar dm253 (DTR 90.0) 2422.57))  ;P127
(setq dm255 (polar ibp (DTR 0.0) 6018.30))  (setq dm256 (polar dm255 (DTR 90.0) 2245.15))  ;P128
(setq dm257 (polar ibp (DTR 0.0) 6232.05))  (setq dm258 (polar dm257 (DTR 90.0) 2245.15))  ;P129
(setq dm259 (polar ibp (DTR 0.0) 6052.95))  (setq dm260 (polar dm259 (DTR 90.0) 2620.70))  ;P130
(setq dm261 (polar ibp (DTR 0.0) 6125.18))  (setq dm262 (polar dm261 (DTR 90.0) 2669.97))  ;P131
(setq dm263 (polar ibp (DTR 0.0) 6363.78))  (setq dm264 (polar dm263 (DTR 90.0) 2646.89))  ;P132
(setq dm265 (polar ibp (DTR 0.0) 6363.78))  (setq dm266 (polar dm265 (DTR 90.0) 2662.28))  ;P133
(setq dm267 (polar ibp (DTR 0.0) 5668.25))  (setq dm268 (polar dm267 (DTR 90.0) 1435.67))  ;P134
(setq dm269 (polar ibp (DTR 0.0) 4226.75))  (setq dm270 (polar dm269 (DTR 90.0) 5640.05))  ;P135

(setq dm271 (polar ibp (DTR 0.0) 1108.33))  (setq dm272 (polar dm271 (DTR 90.0) 1580.12))  ;p136
(setq dm273 (polar ibp (DTR 0.0) 2108.33))  (setq dm274 (polar dm273 (DTR 90.0) 1511.19))  ;p137
(setq dm275 (polar ibp (DTR 0.0) 2045.84))  (setq dm276 (polar dm275 (DTR 90.0) 1580.12))  ;p138
(setq dm277 (polar ibp (DTR 0.0) 2170.83))  (setq dm278 (polar dm277 (DTR 90.0) 1580.12))  ;p139
(setq dm279 (polar ibp (DTR 0.0) 2170.84))  (setq dm280 (polar dm279 (DTR 90.0) 1805.12))  ;p140
(setq dm281 (polar ibp (DTR 0.0) 2170.84))  (setq dm282 (polar dm281 (DTR 90.0) 1930.12))  ;p141

(command "dim1" "ver" dm2 dm4 dm10 (fix expolejoint1)) ;p1 p2
(command "dim1" "ver" dm4 dm6 dm10 (fix expolejoint2)) ;p2 p3
(command "dim1" "ver" dm6 dm8 dm10 (fix expolejoint_end)) ;p3 p4
(command "dim1" "ver" dm10 dm12 dm18 (fix newst1_newmem1_y1)) ;p5 p6
(command "dim1" "ver" dm12 dm14 dm18 (fix (- newstud1_y1 newst1_newmem1_y1))) ;p6 p7
(command "dim1" "ver" dm14 dm16 dm18 (fix (- totalhight newstud1_y1))) ;p7 p8
(command "dim1" "ver" dm18 dm20 (polar dm18 (DTR 180.0) 110) (fix totalhight)) ;p9 p10

(command "dim1" "ver" dm34 dm32 (polar dm32 (DTR 180.0) 55) (rtos newstud1_backmark 2 1)) ;p16 p17
(command "dim1" "ver" dm34 dm36 (polar dm34 (DTR 180.0) 55) (rtos newstud1_backmark 2 1)) ;p17 p18
(command "dim1" "hor" dm38 dm40 (polar dm38 (DTR 270.0) 118) (+ newst1_sizename2 35)) ;p19 p20
(command "dim1" "hor" dm40 dm42 (polar dm42 (DTR 270.0) 129) (- newst1_joint1 100 40 (+ newst1_sizename2 35)) ) ;p20 p21 
(command "dim1" "hor" dm46 dm44 (polar dm46 (DTR 90.0) 66) newst1_sizename2) ;p22 p23
(command "dim1" "ver" dm50 dm48 "@" newst1_sizename1) ;p24 p25
(command "dim1" "hor" dm54 dm56 (polar dm54 (DTR 270.0) 122) (- newst1_joint2 40 (+ newst1_sizename2 35)))  ;p27 p28
(command "dim1" "hor" dm56 dm58 (polar dm56 (DTR 270.0) 137) (+ newst1_sizename2 35)) ;p28 p29
(command "dim1" "ver" dm60 dm62 (polar dm60 (DTR 0.0) 60) newstud1_backmark) ;p30 p31
(command "dim1" "ver" dm62 dm64 (polar dm62 (DTR 0.0) 60) newstud1_backmark) ;p31 p32
(command "dim1" "hor" dm66 dm68 (polar dm68 (DTR 90.0) 85) newst1_sizename2) ;p33 p34
(command "dim1" "ver" dm100 dm102 "@" newst1_sizename1) ;p50 p51
(command "dim1" "ver" dm74 dm72 (polar dm72 (DTR 180.0) 55) (rtos newst1_newmem1_backmark 2 1)) ;p36 p37
(command "dim1" "ver" dm74 dm76 (polar dm76 (DTR 180.0) 55) (rtos newst1_newmem1_backmark 2 1)) ;p37 p38
(command "dim1" "hor" dm78 dm80 (polar dm78 (DTR 270.0) 160) (+ newst1_newmem1_sizename2 35)) ;p39 p40
(command "dim1" "hor" dm80 dm82 (polar dm80 (DTR 270.0) 180) (- newst1_newmem1_len (* (+ newst1_newmem1_sizename2 35) 2))) ;p40 p41
(command "dim1" "hor" dm82 dm84 (polar dm82 (DTR 270.0) 180) (+ newst1_newmem1_sizename2 35)) ;p41 p42
(command "dim1" "hor" dm92 dm94 (polar dm92 (DTR 90.0) 66) newst1_newmem1_sizename2) ;p46 p47
(command "dim1" "hor" dm96 dm98 (polar dm98 (DTR 90.0) 61) newst1_newmem1_sizename2) ;p48 p49
(command "dim1" "ver" dm104 dm106 "@" newst1_newmem1_sizename1) ;p52 p53
(command "dim1" "hor" dm110 dm112 (polar dm112 (DTR 90.0) 90) newst1_sizename2) ;p55 p56
(command "dim1" "ver" dm114 dm116 (polar dm114 (DTR 0.0) 100) (rtos newstud1_backmark 2 1)) ;p57 p58
(command "dim1" "ver" dm116 dm118 (polar dm118 (DTR 0.0) 100) (rtos newstud1_backmark 2 1)) ;p58 p59
(command "dim1" "hor" dm120 dm124 (polar dm120 (DTR 270.0) 63) (+ newst1_sizename2 35)) ;p60 p62
(command "dim1" "hor" dm126 dm128 (polar dm128 (DTR 90.0) 55) newst1_newmem1_sizename2) ;p63 p64
(command "dim1" "ver" dm130 dm132 (polar dm130 (DTR 0.0) 100) (rtos newst1_newmem1_backmark 2 1)) ;p65 p66
(command "dim1" "ver" dm132 dm134 (polar dm134 (DTR 0.0) 100) (rtos newst1_newmem1_backmark 2 1)) ;p66 p67
(command "dim1" "hor" dm136 dm138 (polar dm136 (DTR 270.0) 84) (+ newst1_newmem1_sizename2 35)) ;p68 p69
(command "dim1" "hor" dm140 dm142 (polar dm140 (DTR 90.0) 70) newst1_newmem1_stx2) ;p70 p71 
(command "dim1" "hor" dm144 dm146 (polar dm144 (DTR 270.0) 70) newst1_newmem1_stx1) ;p72 p73
(command "dim1" "hor" dm156 dm158 (polar dm156 (DTR 90.0)79) newstud1_platex1) ;p78 p79
(command "dim1" "hor" dm158 dm160 (polar dm160 (DTR 90.0)79) newstud1_platex2) ;p79 p80
(command "dim1" "ver" dm162 dm164 (polar dm162 (DTR 180.0)79) newstud1_platey1) ;p81 p82
(command "dim1" "ver" dm164 dm166 (polar dm166 (DTR 180.0) 79) (+ newstud1_platey2 50)) ;p82 p83
(command "dim1" "hor" dm172 dm174 (polar dm174 (DTR 90.0) 80) newstud1_clampx1) ;p86 p87
(command "dim1" "hor" dm176 dm178 (polar dm176 (DTR 270.0) 75) newstud1_clmapx2) ;p88 p89
(command "dim1" "hor" dm182 dm184 (polar dm182 (DTR 90.0) 101) newstud1_clampx1) ;p91 p92
(command "dim1" "hor" dm186 dm188 (polar dm186 (DTR 270.0) 75) newstud1_clmapx2) ;p93 p94
(command "dim1" "ver" dm192 dm194 (polar dm194 (DTR 180.0) 90) newstud1_platey1) ;p96 p97
(command "dim1" "ver" dm194 dm196 (polar dm196 (DTR 180.0) 90) newstud1_platey2) ;p97 p98
(command "dim1" "hor" dm196 dm198 (polar dm196 (DTR 90.0) 58) newstud1_platex2) ;p98 p99
(command "dim1" "hor" dm198 dm200 (polar dm200 (DTR 90.0) 58) newstud1_platex1) ;p98 p100
(command "dim1" "ali" dm204 dm206 "@" (strcat "R " (rtos newst1_size 2 1))) ;p102 p103
(command "dim1" "ver" dm208 dm210 (polar dm208 (DTR 0.0) 20) (fix clampstk)) ;p104 p105
(command "dim1" "ali" dm216 dm218 "@" (strcat "R " (rtos newst1_size 2 1))) ;p108 p109
(command "dim1" "ver" dm220 dm222 (polar dm208 (DTR 0.0) 20) (fix clampstk)) ;p110 p111
(command "dim1" "hor" dm226 dm228 (polar dm226 (DTR 90.0) 80) newst1_newmem1_pclampx1) ;p113 p114
(command "dim1" "hor" dm230 dm232 (polar dm230 (DTR 270.0) 80) newst1_newmem1_pclampx2) ;p115 p116
(command "dim1" "hor" dm236 dm238 (polar dm236 (DTR 90.0) 101) newst1_newmem1_pclampx1) ;p118 p119
(command "dim1" "hor" dm240 dm242 (polar dm240 (DTR 270.0) 80) newst1_newmem1_pclampx2) ;p120 p121
(command "dim1" "ali" dm248 dm250 "@" (strcat "R " (rtos newst1_newmem1_size 2 1))) ;p124 p125
(command "dim1" "ver" dm252 dm254 (polar dm208 (DTR 0.0) 20) (fix clampstk)) ;p126 p127
(command "dim1" "ali" dm260 dm262 "@" (strcat "R " (rtos newst1_newmem1_size 2 1))) ;p130 p131
(command "dim1" "ver" dm264 dm266 (polar dm208 (DTR 0.0) 20) (fix clampstk)) ;p132 p133
(command "dim1" "hor" dm272 dm274 (polar dm274 (DTR 270.0) 100) (fix newstud1_x1)) ;p136 p137
(command "dim1" "hor" dm276 dm278 (polar dm278 (DTR 270.0) 70) (fix newstud1_pdwt)) ;p138 p139
(command "dim1" "ver" dm280 dm282 (polar dm280 (DTR 0.0) 130) (fix newstud1_pdht)) ;p140 p141

(setq n3_length (+ newst1_joint1 25))
(setq n3a_length (+ newst1_joint2 25))
(setq n4_length (+ newst1_newmem1_len 50))
(command "_insert" "f3x" dm52 "0.0394" "0.0394" "" "N3" (strcat newstud1_memname ".." (itoa n3_length) " Lg")) ;p26
(command "_insert" "f3x" dm70 "0.0394" "0.0394" "" "N3A" (strcat newstud1_memname ".." (itoa n3a_length) " Lg")) ;p35
(command "_insert" "f3x" dm108 "0.0394" "0.0394" "" "N4" (strcat newst1_newmem1_memname ".." (itoa n4_length) " Lg")) ;p54
(command "_insert" "f3x" dm148 "0.0394" "0.0394" "" "N5" (strcat "PLT..06x60x" (itoa newst1_newmem1_stx1))) ;p74
(command "_insert" "f3x" dm154 "0.0394" "0.0394" "" "N6" (strcat "PL" (rtos flangjointtk 2 0) "x" (rtos (Number_Round (+ newst1_size 200) 1) 2 0) "..." (rtos (Number_Round (+ newst1_size 200) 1) 2 0))) ;p77
(command "_insert" "f3x" dm168 "0.0394" "0.0394" "" "N2" "L150x150x12..150 Lg") ;p84
(command "_insert" "f3x" dm170 "0.0394" "0.0394" "" "N1" (strcat "PL" (rtos bptk 2 0) "x230..230 Lg"))

(command "_insert" "f5x" dm180 "0.0394" "0.0394" "" "L11-1" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newstud1_clmapx2 2 0)))
(command "_insert" "f5x" dm190 "0.0394" "0.0394" "" "L11-2" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newstud1_clmapx2 2 0) ))
(command "_insert" "f5x" dm202 "0.0394" "0.0394" "" "L11-3" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x100x150" ))

(command "_insert" "f5x" dm234 "0.0394" "0.0394" "" "L10-1" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newst1_newmem1_pclampx2 2 0) ))
(command "_insert" "f5x" dm244 "0.0394" "0.0394" "" "L10-2" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x130x" (rtos newst1_newmem1_pclampx2 2 0) ))
(command "_insert" "f5x" dm246 "0.0394" "0.0394" "" "L10-3" (strcat "PLT.." (rtos (fix clampstk) 2 0) "x100x100" ))

(setq N1 (* (/ bptk 1000.0 )(/ 250 1000.0) (/ 250 1000.0) 7850.0))
(setq N2 (* (/ 12 1000.0 )(/ 150 1000.0) (/ 150 1000.0) 7850.0))
(setq N3 (* (/ newst1_joint1 1000.0) newstud1_blockwt 2))
(setq N3A (* (/ newst1_joint2 1000.0) newstud1_blockwt 2))
(setq N4 (/ (* n4_length newst1_newmem1_blockwt 2) 1000.0))
(setq N5 (* (/ 6 1000.0 )(/ 60 1000.0) (/ newst1_newmem1_stx1 1000.0) 7850.0))
(setq N6 (* (/ flangjointtk 1000.0 )(/ (Number_Round (+ newst1_size 200) 1) 1000.0) (/ (Number_Round (+ newst1_size 200) 1) 1000.0) 7850.0))
(setq L11_1 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newstud1_clmapx2 1000.0) 7850.0))
(setq L11_2 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ newstud1_clmapx2 1000.0) 7850.0))
(setq L11_3 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 150 1000.0) 7850.0))
(setq L10_1 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ cc1_d2 1000.0) 7850.0))
(setq L10_2 (* (/ clampstk 1000.0 )(/ 130 1000.0) (/ cc1_d2 1000.0) 7850.0))
(setq L10_3 (* (/ clampstk 1000.0 )(/ 100 1000.0) (/ 100 1000.0) 7850.0))
(setq total_weight1 (+ N1 N2 N3 N3A N4 N5 N6 L11_1 L11_2 L11_3 L10_1 L10_2 L10_3))



(command "style" "TRB" "Trebuchet MS" (* 2.0 sf) "1" "" "" "")
(command "TEXT" dm22 "0" (strcat "%%c" (rtos diameter_1 2 1 ) "x" (rtos diameter_1tick 2 1 ) "Thk"))
(command "TEXT" dm24 "0" (strcat "%%c" (rtos diameter_2 2 1 ) "x" (rtos diameter_2tick 2 1 ) "Thk"))
(command "TEXT" dm26 "0" (strcat "%%c" (rtos diameter_3 2 1 ) "x" (rtos diameter_3tick 2 1 ) "Thk"))
(command "TEXT" dm152 "0" (strcat (rtos newst1_size 2 1) "%%c" " HOLE")) ;p76
(command "TEXT" dm150 "0" (strcat "PCD%%c" (itoa (Number_Round (+ newst1_size 100) 1)))) ;p75
(command "style" "TRB" "Trebuchet MS" (* 3.0 sf) "1" "" "" "" "")
(command "TEXT" dm224 "0" (strcat "%%u" newst1_sizename1))
(command "TEXT" dm268 "0" (strcat "%%u" newst1_newmem1_sizename1))
(setq new_pde_concrite (* (/ newstud_pedestalwidth 1000.0) (/ newstud_pedestalwidth 1000.0) (/ newstud_pedestalhight 1000.0)))
(setq iplan dm30)
	(if (and (= newstud1_memty "P") (> newstud1_memlen 6000) (= newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/PPJ90" iplan "" "" "" )))
	(if (and (= newstud1_memty "P") (< newstud1_memlen 6000) (= newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/PP90" iplan "" "" "" )))
	(if (and (= newstud1_memty "P") (> newstud1_memlen 6000) (< newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/PPJ30" iplan "" "" "" )))
	(if (and (= newstud1_memty "P") (< newstud1_memlen 6000) (< newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/PP30" iplan "" "" "" )))
	(if (and (= newstud1_memty "P") (> newstud1_memlen 6000) (> newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/PPJ120" iplan "" "" "" )))
	(if (and (= newstud1_memty "P") (< newstud1_memlen 6000) (> newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/PP120" iplan "" "" "" )))
		
	(if (and (= newstud1_memty "A") (> newstud1_memlen 6000) (= newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/AAJ90" iplan "" "" "" )))
	(if (and (= newstud1_memty "A") (< newstud1_memlen 6000) (= newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/AA90" iplan "" "" "" )))
	(if (and (= newstud1_memty "A") (> newstud1_memlen 6000) (< newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/AAJ30" iplan "" "" "" )))
	(if (and (= newstud1_memty "A") (< newstud1_memlen 6000) (< newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/AA30" iplan "" "" "" )))
	(if (and (= newstud1_memty "A") (> newstud1_memlen 6000) (> newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/AAJ120" iplan "" "" "" )))
	(if (and (= newstud1_memty "A") (< newstud1_memlen 6000) (> newtwost_ang 90)) (progn (command "_insert" "c:/detail/bolckss/AA120" iplan "" "" "" )))

))
(setq next1 (polar ibp (dtr 0.0) 9900))
(setq ibp next1)
))
))
)