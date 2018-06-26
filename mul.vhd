Library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.STD_LOGIC_MISC.all;
use IEEE.STD_LOGIC_ARITH.all;

----------------integer register--------------------

Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;


Entity Reg is
      Generic ( Delay:   Time := 4 ns ); 
      Port (    Clk : In    std_logic;
                Din : In    INTEGER;
                Rst:  In    std_logic;
                Ld : In    std_logic;
                Dout : Out   INTEGER );
End Reg;

Architecture BEHAVIORAL of Reg is

   Begin
    P: Process (Clk)
    Variable Value : INTEGER := 0;
    Begin
     if( Clk'event and Clk = '1' ) then
       if (Rst = '1') then
           Dout <= 0;           
       elsif( Ld = '1' ) then
           Dout <=  Din after Delay;
       End if;
     End if; 
    End Process P;
End BEHAVIORAL;

-----------256x32(integer) Register file-------------------

Library ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_unsigned.ALL;
 
Entity RegFile IS
   Generic ( Delay:   Time := 8 ns );
   Port (R_addr1,R_addr2,W_addr: IN std_logic_vector(7 DOWNTO 0);
         R_en1,R_en2, W_en: IN std_logic;
         R_data1, R_data2: OUT INTEGER; 
         W_data: IN INTEGER; 
         Clk: IN std_logic );
End RegFile;

Architecture Behavioral OF RegFile IS 
    type RF is array ( 0 to 31, 0 to 7 ) of INTEGER;
    signal Storage : RF := (
             --------OutBlock--------
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
             --------TempBlock--------
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
             --------InBlock--------
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
            ( 0, 0, 0, 0, 0, 0, 0, 0 ),
           --------COS-------------
            ( 125, 122,  115, 103,  88,  69,   47,   24  ),
            ( 125, 103,  47,  -24,  -88, -122, -115, -69  ),
            ( 125, 69,   -47, -122, -88, 24,   115,  103  ),
            ( 125, 24,   -115, -69, 88,  103,  -47,  -122  ),
            ( 125, -24,  -115, 69,  88,  -103, -47,  122  ),
            ( 125, -69,  -47, 122,  -88, -24,  115,  -103  ),
            ( 125, -103, 47,  24,   -88, 122,  -115, 69  ),
            ( 125, -122, 115, -103, 88,  -69,  47,   -24 )          
            );
Begin
    WriteProcess: Process(Clk)
    Variable col_w:std_logic_vector(2 DOWNTO 0);
    Variable row_w:std_logic_vector(4 DOWNTO 0);
    Begin
          row_w := W_addr(7 downto 3);
          col_w := W_addr(2 downto 0);
        
    if( Clk'event and Clk = '1' ) then   
      if(W_en = '1') then 
        -- write --
        Storage( CONV_INTEGER(row_w), CONV_INTEGER(col_w)) <= W_data after Delay;
      End if;
        
    End if;
    
    End Process;

    ReadProcess: Process(R_en1, R_addr1, R_en2, R_addr2,Storage)
    Variable  col_r1, col_r2:std_logic_vector(2 DOWNTO 0);  
    Variable  row_r1, row_r2:std_logic_vector(4 DOWNTO 0);
    Begin
        row_r1 := R_addr1(7 downto 3);
        col_r1 := R_addr1(2 downto 0);
        row_r2 := R_addr2(7 downto 3);
        col_r2 := R_addr2(2 downto 0);
    
    if(R_en1 = '1') then 
        R_data1 <= Storage( CONV_INTEGER(row_r1), CONV_INTEGER(col_r1) ) after Delay;
    else
        R_data1 <= INTEGER'left;  --huge negative  
    End if;
    
    if(R_en2 = '1') then 
        R_data2 <= Storage( CONV_INTEGER(row_r2), CONV_INTEGER(col_r2) ) after Delay;
    else
        R_data2 <= INTEGER'left;    
    End if;
    End Process;
End Behavioral;


-------------------------------Counter------------------------------

Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;



Entity Counter is
      Generic ( Delay:   Time := 8 ns );
      Port (    Clk : In    std_logic;
                Inc : In    std_logic;
                Rst : In    std_logic;
                i : Out   std_logic_vector(2 downto 0);
                j : Out   std_logic_vector(2 downto 0);
                k : Out   std_logic_vector(2 downto 0) );
End Counter;

Architecture BEHAVIORAL of Counter is

   Begin
    P: Process ( Clk )
    Variable Value : UNSIGNED( 8 downto 0 ) := "000000000";
    Begin
        if( Clk'event and Clk = '1' ) then 
            if( Rst = '1' ) then
                Value := "000000000";
            elsif( Inc = '1' ) then
                Value := Value + 1;
            End if;
        End if;
                i(2) <= Value(8) after Delay;
                i(1) <= Value(7) after Delay;
                i(0) <= Value(6) after Delay;
                j(2) <= Value(5) after Delay; 
                j(1) <= Value(4) after Delay;
                j(0) <= Value(3) after Delay;
                k(2) <= Value(2) after Delay;
                k(1) <= Value(1) after Delay; 
                k(0) <= Value(0) after Delay;                    
    End Process P;

End BEHAVIORAL;

---------------------------Multiplier--------------------------

library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_misc.all;
   use IEEE.std_logic_arith.all;


entity Multiplier is
          generic ( Delay:   Time := 25 ns );
       Port (A : In    integer;
             B : In    integer;
             Product : Out   integer );
end Multiplier;

architecture BEHAVIORAL of Multiplier is
   begin
    P: process ( A, B )
        variable Result : integer := 0;
    begin
        if( A /= integer'left and B /= integer'left ) then
            Result := A * B;
        end if;
        Product <= Result after Delay;
    end process P;
end BEHAVIORAL;


-------------------------Adder-------------------------------

library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_misc.all;
   use IEEE.std_logic_arith.all;


entity Adder is
         generic ( Delay:   Time := 15 ns ); 
      Port (     A : In    integer;
                 B : In    integer;
                 Sum : Out   integer );
end Adder;

architecture BEHAVIORAL of Adder is

   begin
    P: process ( A, B )
        variable Result : integer := 0;
    begin
        if( A /= integer'left and B /= integer'left ) then
            Result := A + B;
        end if;
        Sum <= Result after Delay;
    end process P;

end BEHAVIORAL;


----------------------32-bit(integer) Mux----------------------

Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;



Entity MuxInt is
      Generic ( Delay:   Time := 4 ns );
      Port (    C : In    std_logic;
                D0 : In    INTEGER;
                D1 : In    INTEGER;
                Dout : Out   INTEGER );
End MuxInt;

Architecture BEHAVIORAL of MuxInt is
   Begin
    P: Process ( D0, D1, C )
    Variable data_out : INTEGER;
    Begin
        if( C = '0' ) then
            data_out := D0;
        else
            data_out := D1;
        End if;
        Dout <= data_out after Delay;
    End Process P;
End BEHAVIORAL;


--------------------3-bit Mux--------------------------
Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;



Entity Mux3 is
      Generic ( Delay:   Time := 4 ns );
      Port (    C : In    std_logic;
                D0 : In    std_logic_vector(2 downto 0);
                D1 : In    std_logic_vector(2 downto 0);
                Dout : Out   std_logic_vector(2 downto 0) );
End Mux3;

Architecture BEHAVIORAL of Mux3 is

   Begin
    P: Process ( D0, D1, C )
        Begin
        if( C = '0' ) then
            Dout <= D0 after Delay;
        else
            Dout <= D1 after Delay;
        End if;
        
    End Process P;
End BEHAVIORAL;

---------- 32-bit (integer) Three-state Buffer ----------

Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;


Entity ThreeStateBuff IS
    Generic ( Delay:   Time := 1 ns );
    Port (Control_Input: IN std_logic;
          Data_Input: IN INTEGER;
          Output: OUT INTEGER );
End ThreeStateBuff;

Architecture Beh OF ThreeStateBuff IS
Begin
    Process (Control_Input, Data_Input)
    Begin
        IF (Control_Input = '1') THEN
            Output <= Data_Input AFTER Delay;
        ELSE
            Output <= INTEGER'left AFTER Delay;
        End IF;
    End Process;
End Beh;

  

 -------------------------------------------------------------
 --      Controller
 -------------------------------------------------------------
Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;



Entity Controller is
     Generic ( Delay:   Time := 6.4 ns );
     Port (  Clk   : In    std_logic;
             Start : In    std_logic;
             Count : In    std_logic_vector(8 downto 0);
             Inc : Out   std_logic := '0';
             R_en1 : Out   std_logic := '0';
             R_en2 : Out   std_logic := '0';
             W_en : Out   std_logic := '0';
             LoadSum : Out   std_logic := '0';               
             Rst_counter : Out   std_logic := '0';
             Rst_sum : Out   std_logic := '0'; 
             Rst_p : Out   std_logic := '0'; 
             Sel1 : Out   std_logic := '0';
             Sel2 : Out   std_logic := '0';
             Sel3 : Out   std_logic := '0';
             Sel4 : Out   std_logic := '0';
             Sel5 : Out   std_logic := '0';
             Sel6 : Out   std_logic := '0';
             Sel7 : Out   std_logic := '0'; 
             Oe   : Out   std_logic := '0';                  
             Done : Out   std_logic := '0');
End Controller;

Architecture BEHAVIORAL of Controller is

  -- you can have as many states as necessary
  -- no need to encode unless you really want to
   type STATE_VALUE is (S_0,S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8); 
   SIGNAL CurrState, NextState : STATE_VALUE;
   

begin
 StateReg: process (Clk)
           begin
              if(Clk'event and Clk='1') then
                CurrState <= NextState after Delay;
              end if;
           end process StateReg;
 
 CombLogic: process(CurrState, Start, Count)
            variable  VarDone : std_logic;
            variable  i, j, k : INTEGER;
           
        begin
           
            if( Count(0) /= 'U' and Start /= 'U' ) then
                i := CONV_INTEGER( unsigned(Count( 8 downto 6 )) );
                j := CONV_INTEGER( unsigned(Count( 5 downto 3 )) );
                k := CONV_INTEGER( unsigned(Count( 2 downto 0 )) );
            end if;
            
            case CurrState is
            
            When S_0 =>
                -- Wait until start is 1
                Rst_counter <= '0';
                Done <= '0';
                Sel4 <= '0';
                Sel5 <= '0';
                if (Start='1') then
                    Nextstate <= S_1 after DELAY;
                else
                    Nextstate <= S_0 after DELAY;
                end if; 
                
            When S_1 =>
                -- InBlock <= Din
                Inc <= '1';
                R_en1 <= '0';
                R_en2 <= '0';
                W_en <= '1';
                Sel1 <= '0';
--                Sel6 <= '1';
--                Sel7 <= '0';
                Sel4 <= '0';
                Sel5 <= '0';
                
                if (i=1) then
                    Rst_counter <= '1' after DELAY;
                    Nextstate <= S_2 after DELAY;
                else
                    Nextstate <= S_1 after DELAY;
                end if;
                
            When S_2 =>
                -- TempBlock <= COS1*f
                Rst_counter <= '0';
                Inc <= '1';
                R_en1 <= '1';
                R_en2 <= '1';
                W_en <= '1';
                Sel1 <= '1';
--                Sel6 <= '0';
--                Sel7 <= '1';
                -- 2,3 determines which comes first in multiplication
                Sel2 <= '0';
                Sel3 <= '1';
                -- 4,5 determines first two bits w_addr
                Sel4 <= '0';
                Sel5 <= '1';
                
                Sel6 <= '0';
                if (k=7) then
                    Sel6 <= '1';
                end if;
                
                if (i=7 and j=7 and k=7) then
                    Rst_counter <= '1' after DELAY;
                    Nextstate <= S_3 after DELAY;
                else
                    Nextstate <= S_2 after DELAY;
                end if;

            When S_3 =>
                -- OutBlock <= Temp*COS2
                Rst_counter <= '0';
                Inc <= '1';
                R_en1 <= '1';
                R_en2 <= '1';
                W_en <= '1';
                Sel1 <= '1';
--                Sel6 <= '0';
--                Sel7 <= '0';

                Sel2 <= '0';
                Sel3 <= '1';
                
                Sel4 <= '1';
                Sel5 <= '0';
                
                Sel6 <= '0';
                
                if (k=7) then
                    Sel6 <= '1';
                end if;
                                
                if (i=7 and j=7 and k=7) then
                    Rst_counter <= '1' after DELAY;
                    Nextstate <= S_4 after DELAY;
                else
                    Nextstate <= S_3 after DELAY;
                end if;                
                
            When S_4 =>
                -- output data
                Rst_counter <= '0';
                Inc <= '1';
                R_en1 <= '1';
                R_en2 <= '0';
                W_en <= '0';
                Sel1 <= '0';
--                Sel6 <= '1';
--                Sel7 <= '0';
                Sel4 <= '1';
                Sel5 <= '1';
                
                -- For outreg and buff
                Sel6 <= '1';
                Oe <= '1';
                
                
                if (i=1) then
                    Rst_counter <= '1' after DELAY;
                    Done <= '1' after DELAY;
                    Nextstate <= S_0 after DELAY;
                else
                    Nextstate <= S_4 after DELAY;
                end if;                
                
                    
            
            When Others =>
                
                
            end case;
        end process; 
End BEHAVIORAL;

 -------------------------------------------------------------
 --      top level: structure for DCT
 --      minimal clock cycle = ??? ns
 -------------------------------------------------------------
 
Library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_misc.all;
use IEEE.std_logic_arith.all;


Entity DCT_str IS
      Port (
                Clk :           in std_logic;
                Start :         in std_logic;
                Din :           in INTEGER;
                Done :          out std_logic;
                Dout :          out INTEGER
              );
End DCT_str;


Architecture struct OF DCT_str IS

COMPONENT Multiplier IS
      PORT ( A : In    integer;
             B : In    integer;
             Product : Out   integer );
END COMPONENT;

COMPONENT Adder IS
      PORT (     A : In    integer;
                 B : In    integer;
                 Sum : Out   integer );
END COMPONENT;

COMPONENT Counter IS
      Port (    Clk : In    std_logic;
                Inc : In    std_logic;
                Rst : In    std_logic;
                i : Out   std_logic_vector(2 downto 0);
                j : Out   std_logic_vector(2 downto 0);
                k : Out   std_logic_vector(2 downto 0)  );
End COMPONENT;

COMPONENT MuxInt is
       Port (    C : In    std_logic;
                D0 : In    INTEGER;
                D1 : In    INTEGER;
                Dout : Out   INTEGER );
End COMPONENT;

COMPONENT Mux3 is
      Port (    C : In    std_logic;
                D0 : In    std_logic_vector(2 downto 0);
                D1 : In    std_logic_vector(2 downto 0);
                Dout : Out   std_logic_vector(2 downto 0) );
End COMPONENT;

COMPONENT Reg is
      Port (    Clk : In    std_logic;
                DIN : In    INTEGER;
                Rst : In    std_logic;
                Ld : In    std_logic;
                Dout : Out   INTEGER );
End COMPONENT;

COMPONENT RegFile IS
      Port (    R_addr1,R_addr2,W_addr: IN std_logic_vector(7 DOWNTO 0);
                R_en1,R_en2, W_en: IN std_logic;
                R_data1, R_data2: OUT INTEGER; 
                W_data: IN INTEGER; 
                Clk: IN std_logic );
End COMPONENT;

COMPONENT ThreeStateBuff IS
      Port (    Control_Input: IN std_logic;
                Data_Input: IN INTEGER;
                Output: OUT INTEGER );
End COMPONENT;

COMPONENT Controller IS
      Port (    Clk   : In    std_logic;
                Start : In    std_logic;
                Count : In    std_logic_vector(8 downto 0);
                Inc : Out   std_logic ;
                R_en1 : Out   std_logic ;
                R_en2 : Out   std_logic ;
                W_en : Out   std_logic ;
                LoadSum : Out   std_logic ;              
                Rst_counter : Out   std_logic;
                Rst_sum : Out   std_logic; 
                Rst_p : Out   std_logic := '0'; 
                Sel1 : Out   std_logic;
                Sel2 : Out   std_logic ;
                Sel3 : Out   std_logic ;
                Sel4 : Out   std_logic ;
                Sel5 : Out   std_logic ;
                Sel6 : Out   std_logic ;
                Sel7 : Out   std_logic ;
                Oe   : Out   std_logic ;                 
                Done : Out   std_logic );
End COMPONENT;

--------------------------------------------------
--you may modify below signals or declare new ones  
--for the interconnections of the structural model
--------------------------------------------------

SIGNAL Inc, Rst_counter, Rst_sum, Rst_p:std_logic;
SIGNAL R_en1_s, R_en2_s :std_logic;
SIGNAL W_en_s :std_logic;
SIGNAL Sel1_s, Sel2_s,Sel3_s,Sel4_s,Sel5_s,Sel6_s,Sel7_s:std_logic;
SIGNAL muxout1,muxout2,muxout3: std_logic_vector(2 downto 0);
SIGNAL muxout4, muxout5, muxout6: INTEGER;
SIGNAL mult_out: INTEGER;
SIGNAL add_out: INTEGER;
SIGNAL R_data1_out, R_data2_out : INTEGER;
SIGNAL i_s, j_s, k_s: std_logic_vector(2 downto 0);
SIGNAL LoadSum:std_logic;
SIGNAL reg_sum_out,reg_p_out,reg_a_out,reg_b_out: INTEGER;
SIGNAL Oe_s :std_logic;

SIGNAL count_s: std_logic_vector(8 downto 0);
SIGNAL R_addr1s: std_logic_vector(7 downto 0);
SIGNAL R_addr2s: std_logic_vector(7 downto 0);
SIGNAL W_addrs: std_logic_vector(7 downto 0);

SIGNAL Result1: INTEGER;
SIGNAL Result2: INTEGER;

type ADDR is array (0 to 3) of STD_LOGIC_VECTOR(7 downto 0);
signal W_addr_s : ADDR :=("00000000","00000000","00000000","00000000");
signal R_addr_s1 : ADDR :=("00000000","00000000","00000000","00000000");
signal R_addr_s2 : ADDR :=("00000000","00000000","00000000","00000000");

signal addrloc: std_logic_vector(1 downto 0);

Begin

    count_s <= i_s&j_s&k_s;
    -- During S_1 (input)
    -- muxout1 = j; muxout2 = k;
    
    -- During S_2 (first multiplication)
    -- muxout1 = i; muxout2 = j; muxout3 = k
    
    -- During S_3  (second multiplication)
    -- muxout1 = ; muxout2 = ; muxout3 = 
     
    -- During S_4 (output)
    -- muxout1 = j; muxout2 = k;
    
    addrloc <= sel4_s&sel5_s; 
    
    W_addr_s <= ("10"&j_s&k_s,"01"&i_s&j_s,"00"&i_s&j_s,"00000000");
    R_addr_s1 <= ("00000000","11"&i_s&k_s,"01"&i_s&k_s,"00"&j_s&k_s);
    R_addr_s2 <= ("00000000","10"&k_s&j_s,"11"&j_s&k_s,"00000000");
    
    W_addrs <= W_addr_s(CONV_INTEGER(unsigned(addrloc)));
    R_addr1s <= R_addr_s1(CONV_INTEGER(unsigned(addrloc)));
    R_addr2s <= R_addr_s2(CONV_INTEGER(unsigned(addrloc)));
    
    
    MuxIntb: MuxInt port map(
    C => Sel2_s, D0 => R_data1_out, D1 => R_data2_out, 
    Dout => muxout5
    );
    
    MuxIntc: MuxInt port map(
    C => Sel3_s, D0 => R_data1_out, D1 => R_data2_out, 
    Dout => muxout6 
    );    
    
    
    Multipliera: Multiplier port map(
    A => muxout5, B=> muxout6,
    Product => mult_out
    );
    
    
    Addera: Adder port map(
    A => reg_b_out, B => mult_out,
    Sum => add_out
    );
    
    Regb: Reg port map(
    Clk => Clk, DIN => add_out, Rst => Sel6_s,
    Ld => '1', Dout => reg_b_out
    );
 
    MuxInta: MuxInt port map(
    C => Sel1_s, D0 => Din, D1 => add_out, 
    Dout => muxout4
    );
    
            
    Control: Controller port map(
    Clk => Clk, Start => Start,
    Count => count_s, Inc => Inc,
    R_en1 => R_en1_s, R_en2 => R_en2_s,
    W_en => W_en_s, LoadSum => LoadSum,
    Rst_counter => Rst_counter, Rst_sum => Rst_sum, Rst_p => Rst_p,
    Sel1 => Sel1_s, Sel2 => Sel2_s, Sel3 => Sel3_s, Sel4 => Sel4_s,
    Sel5 => Sel5_s, Sel6 => Sel6_s, Sel7 => Sel7_s,
    Oe => Oe_s, Done => Done);
    
    Count: Counter port map(
    Clk => Clk, Inc => Inc,
    Rst => Rst_counter,
    i => i_s, j => j_s, k => k_s);
    
    RF: RegFile port map (
    R_addr1 => R_addr1s, R_addr2 => R_addr2s, W_addr => W_addrs,
    R_en1 => R_en1_s, R_en2 => R_en2_s, W_en => W_en_s,
    R_data1 => R_data1_out, R_data2 => R_data2_out,
    W_data => muxout4, Clk => Clk);
    
    Rega: Reg port map(
    Clk => Clk, DIN => R_data1_out, Rst => '0',
    Ld => Sel6_s, Dout => reg_a_out
    );
    
    Buffa: ThreeStateBuff port map(
    Control_Input => Oe_s, Data_Input => reg_a_out, Output => Dout);
    

End struct;
 
