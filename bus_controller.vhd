library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity BusController is

  port (
    clk, rst                               : in    std_logic;
    busReq                                 : in    std_logic_vector(N_CACHES-1 downto 0);
    busAddr                                : in    std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    busCmd                                 : in    bus_cmd_t;
    busData                                : inout data_block_t;
    busGrant                               : out   std_logic_vector(N_CACHES-1 downto 0);
    memCs, memRead, memWrite, memWriteWord : out   std_logic;
    memAddr                                : out   std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    memWrData                              : out   data_block_t;
    memRdData                              : in    data_block_t;
    memDone                                : in    std_logic);

end entity BusController;

architecture rtl of BusController is
  type bus_state_t is (ST_IDLE, ST_GRANT, ST_WAIT_MEM);
  signal busSt, busStNext : bus_state_t := ST_IDLE;

  signal arbiterArbitrate : std_logic;
  signal arbiterReqValid  : std_logic;
  signal arbiterReqId     : std_logic_vector(CACHE_IDX_WIDTH-1 downto 0);

  -- Intern signal used to control the tri state buffer
  signal busOutEn         : std_logic;
  
begin  -- architecture rtl

  comb_proc : process (arbiterReqValid, memDone) is
  begin  -- process comb_proc
    -- internal signals default values
    busStNext <= busSt;
    arbiterArbitrate <= '0';
    busOutEn <= '0';

    -- outputs default values
    busGrant <= '0';
    memCs <= '0';

    -- signal with dont care initialization here

    -- control: state machine
    case busSt is
      when ST_IDLE =>
        if (arbiterReqValid = '1') then
          busStNext <= ST_GRANT;
        end if;

      when ST_GRANT =>
        busStNext <= ST_WAIT_MEM;
        arbiterArbitrate <= '1';
        busGrant(to_integer(unsigned(arbiterReqId))) <= '1';
        memCs <= '1';

      when ST_WAIT_MEM =>
        if (memDone = '0') then
          -- Do not update the state
          busGrant(to_integer(unsigned(arbiterReqId))) <= '1';
        else
          busStNext <= ST_IDLE;
          busOutEn <= '1';
        end if;

      when others => null;
    end case;

    -- datapath
  end process comb_proc;

  busArbiter_1 : busArbiter
    port map (
      clk              => clk,
      arbiterArbitrate => arbiterArbitrate,
      arbiterBusReqIn  => busReq,
      arbiterReqValid  => arbiterReqValid,
      arbiterReqId     => arbiterReqId);

  BusTriStateBuffer_1 : BusTriStateBuffer
    port map (
      busOutEn   => busOutEn,
      busDataIn  => memRdData,
      busData    => busData);

  
  clk_proc : process (clk, rst) is
  begin  -- process clk_proc
    if rst = '0' then                   -- asynchronous reset (active low)
      busSt <= ST_IDLE;
    elsif clk'event and clk = '1' then  -- rising clock edge
      busSt <= busStNext;
    end if;
  end process clk_proc;

end architecture rtl;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity BusTriStateBuffer is

  port (
    busOutEn  : in  std_logic;
    busDataIn : in  data_block_t;
    busData   : out data_block_t);

end entity BusTriStateBuffer;

architecture tsb of BusTriStateBuffer is

begin
    busData <= busDataIn when (busOutEn = '1') else 'Z';
end tsb;
