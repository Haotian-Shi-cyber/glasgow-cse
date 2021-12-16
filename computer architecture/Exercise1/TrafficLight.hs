module Trafficlight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

trafficlight1 :: CBit a => a->(a,a,a)
trafficlight1 reset = (green, amber, red)
    where
        green = or3 g1 g2 g3
        amber = or2 a1 a2
        red = or4 r1 r2 r3 r4
        g1 = dff(or2 reset a2) 
        g2 = dff g1
        g3 = dff g2
        a1 = dff g3
        r1 = dff a1
        r2 = dff r1
        r3 = dff r2
        r4 = dff r3
        a2 = dff r4

trafficlight2 :: CBit a => a -> a -> (a,a,a,a,a,[a])
trafficlight2 reset walkrequest = (green, amber, red, wait, walk, walkCount)
        where
        green = or2 g1 reg_light'
        amber = or2 a1 a2
        red = or3 r1 r2 r3
        wait = or4 wt1 wt2 reg_walk' wt3
        walk = or3 wk1 wk2 wk3
        --walk/wait circuit design
        w_input = and2 walkrequest reg_walk'
        wt1 = dff w_input
        wk1 = dff wt1
        wk2 = dff wk1
        wk3 = dff wk2
        wt2 = dff wk3
        wt3 = dff wt2
        op = or2 walkrequest wt3
        reg_walk = dff (mux1 op reg_walk walkrequest)
        reg_walk' = inv reg_walk
        -- green amber red circuit design
        l_input = and2 walkrequest reg_light'
        a1 = dff l_input
        r1 = dff a1
        r2 = dff r1
        r3 = dff r2
        a2 = dff r3
        g1 = dff a2
        op_light = or2 g1 walkrequest
        reg_light = dff (mux1 op_light reg_light walkrequest)
        reg_light' = inv reg_light
        walkCount = counter16 reset walkrequest
-- walk counter circuit design
counter16 :: CBit a=>a->a->[a]
counter16 reset walkrequest = [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15]
    where
        x0 = dff (mux1 reset y0 zero)
        x1 = dff (mux1 reset y1 zero)
        x2 = dff (mux1 reset y2 zero)
        x3 = dff (mux1 reset y3 zero)
        x4 = dff (mux1 reset y4 zero)
        x5 = dff (mux1 reset y5 zero)
        x6 = dff (mux1 reset y6 zero)
        x7 = dff (mux1 reset y7 zero)
        x8 = dff (mux1 reset y8 zero)
        x9 = dff (mux1 reset y9 zero)
        x10 = dff (mux1 reset y10 zero)
        x11 = dff (mux1 reset y11 zero)
        x12 = dff (mux1 reset y12 zero)
        x13 = dff (mux1 reset y13 zero)
        x14 = dff (mux1 reset y14 zero)
        x15 = dff (mux1 reset y15 zero)

        (cout, [y0,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15]) = rippleAdd zero [(x0,zero),(x1,zero),(x2,zero),(x3,zero),(x4,zero),(x5,zero),(x6,zero),(x7,zero),(x8,zero),(x9,zero),(x10,zero),(x11,zero),(x12,zero),(x13,zero),(x14,zero),(x15,walkrequest)]