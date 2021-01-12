-- Generated by Lava 2000

use work.all;

entity
  4BitAdder_row
is
port
  -- clock
  ( clk : in bit

  -- inputs
  ; cin : in bit
  ; a_0 : in bit
  ; b_0 : in bit
  ; a_1 : in bit
  ; b_1 : in bit
  ; a_2 : in bit
  ; b_2 : in bit
  ; a_3 : in bit
  ; b_3 : in bit
  ; a_4 : in bit
  ; b_4 : in bit

  -- outputs
  ; sum_0 : out bit
  ; sum_1 : out bit
  ; sum_2 : out bit
  ; sum_3 : out bit
  ; cout : out bit
  );
end entity 4BitAdder_row;

architecture
  structural
of
  4BitAdder_row
is
  signal w1 : bit;
  signal w2 : bit;
  signal w3 : bit;
  signal w4 : bit;
  signal w5 : bit;
  signal w6 : bit;
  signal w7 : bit;
  signal w8 : bit;
  signal w9 : bit;
  signal w10 : bit;
  signal w11 : bit;
  signal w12 : bit;
  signal w13 : bit;
  signal w14 : bit;
  signal w15 : bit;
  signal w16 : bit;
  signal w17 : bit;
  signal w18 : bit;
  signal w19 : bit;
  signal w20 : bit;
  signal w21 : bit;
  signal w22 : bit;
  signal w23 : bit;
  signal w24 : bit;
  signal w25 : bit;
  signal w26 : bit;
  signal w27 : bit;
  signal w28 : bit;
  signal w29 : bit;
  signal w30 : bit;
  signal w31 : bit;
  signal w32 : bit;
  signal w33 : bit;
  signal w34 : bit;
  signal w35 : bit;
  signal w36 : bit;
begin
  c_w2      : entity id    port map (clk, cin, w2);
  c_w4      : entity id    port map (clk, a_0, w4);
  c_w5      : entity id    port map (clk, b_0, w5);
  c_w3      : entity xor2  port map (clk, w4, w5, w3);
  c_w1      : entity xor2  port map (clk, w2, w3, w1);
  c_w8      : entity and2  port map (clk, w2, w3, w8);
  c_w9      : entity and2  port map (clk, w4, w5, w9);
  c_w7      : entity xor2  port map (clk, w8, w9, w7);
  c_w11     : entity id    port map (clk, a_1, w11);
  c_w12     : entity id    port map (clk, b_1, w12);
  c_w10     : entity xor2  port map (clk, w11, w12, w10);
  c_w6      : entity xor2  port map (clk, w7, w10, w6);
  c_w15     : entity and2  port map (clk, w7, w10, w15);
  c_w16     : entity and2  port map (clk, w11, w12, w16);
  c_w14     : entity xor2  port map (clk, w15, w16, w14);
  c_w18     : entity id    port map (clk, a_2, w18);
  c_w19     : entity id    port map (clk, b_2, w19);
  c_w17     : entity xor2  port map (clk, w18, w19, w17);
  c_w13     : entity xor2  port map (clk, w14, w17, w13);
  c_w22     : entity and2  port map (clk, w14, w17, w22);
  c_w23     : entity and2  port map (clk, w18, w19, w23);
  c_w21     : entity xor2  port map (clk, w22, w23, w21);
  c_w25     : entity id    port map (clk, a_3, w25);
  c_w26     : entity id    port map (clk, b_3, w26);
  c_w24     : entity xor2  port map (clk, w25, w26, w24);
  c_w20     : entity xor2  port map (clk, w21, w24, w20);
  c_w29     : entity and2  port map (clk, w21, w24, w29);
  c_w30     : entity and2  port map (clk, w25, w26, w30);
  c_w28     : entity xor2  port map (clk, w29, w30, w28);
  c_w32     : entity id    port map (clk, a_4, w32);
  c_w33     : entity id    port map (clk, b_4, w33);
  c_w31     : entity xor2  port map (clk, w32, w33, w31);
  c_w27     : entity xor2  port map (clk, w28, w31, w27);
  c_w35     : entity and2  port map (clk, w28, w31, w35);
  c_w36     : entity and2  port map (clk, w32, w33, w36);
  c_w34     : entity xor2  port map (clk, w35, w36, w34);

  -- naming outputs
  c_sum_0   : entity id    port map (clk, w1, sum_0);
  c_sum_1   : entity id    port map (clk, w6, sum_1);
  c_sum_2   : entity id    port map (clk, w13, sum_2);
  c_sum_3   : entity id    port map (clk, w20, sum_3);
  c_cout    : entity id    port map (clk, w27, cout);
end structural;
