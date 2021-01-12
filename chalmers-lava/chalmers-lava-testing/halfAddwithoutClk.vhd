library ieee;

use ieee.std_logic_1164.all;

entity
  halfAddwithoutClk
is
port
  ( 
 
    
    inp_1 : in std_logic
  ; inp_2 : in std_logic

  
  ; outp_0 : out std_logic
  ; outp_1 : out std_logic
  );
end halfAddwithoutClk;

architecture
  structural
of
  halfAddwithoutClk
is
  signal w1 : std_logic;
  signal w2 : std_logic;
  signal w3 : std_logic;
  signal w4 : std_logic;
begin
  c_w2      : entity work.wire  port map (inp_1, w2);
  c_w3      : entity work.wire  port map (inp_2, w3);
  c_w1      : entity work.xorG  port map (w2, w3, w1);
  c_w4      : entity work.andG  port map (w2, w3, w4);

  
  c_outp_0  : entity work.wire  port map (w1, outp_0);
  c_outp_1  : entity work.wire  port map (w4, outp_1);
end structural;
