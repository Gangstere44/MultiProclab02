library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity CacheController is

  port (
    clk, rst                       : in    std_logic;
    cacheCs, cacheRead, cacheWrite : in    std_logic;
    cacheAddr                      : in    std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    cacheWrData                    : in    data_word_t;
    cacheDone                      : out   std_logic;
    cacheRdData                    : out   data_word_t;
    busReq                         : out   std_logic;
    busCmd                         : out   bus_cmd_t;
    busGrant                       : in    std_logic;
    busAddr                        : out   std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
    busData                        : inout data_block_t);
end entity CacheController;

architecture rtl of CacheController is
  type cache_ctrl_state_t is (ST_IDLE, ST_RD_HIT_TEST, ST_RD_WAIT_BUS_GRANT_ACC,
                              ST_RD_WAIT_BUS_COMPLETE_ACC, ST_RD_WAIT_BUS_GRANT_WB,
                              ST_RD_WAIT_BUS_COMPLETE_WB,
                              ST_WR_HIT_TEST, ST_WR_WAIT_BUS_GRANT, ST_WR_WAIT_BUS_COMPLETE);

  signal cacheStNext, cacheSt                : cache_ctrl_state_t := ST_IDLE;

  signal tagLookupEn, tagWrEn, tagWrSetDirty : std_logic;
  signal tagWrSet                            : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal tagAddr                             : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal tagHitEn, tagVictimDirty            : std_logic;
  signal tagHitSet, tagVictimSet             : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal tagVictimAddr                       : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);

  signal dataArrayWrEn, dataArrayWrWord : std_logic;
  signal dataArrayWrSetIdx              : std_logic_vector(WORD_OFFSET_WIDTH-1 downto 0);
  signal dataArrayAddr                  : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal dataArrayWrData                : data_block_t;
  signal dataArrayRdData                : data_set_t;

  -- begin ADDED internal signal
  
  signal busDataWord 			: data_word_t;
  
  signal tmpDataArrayRdData 	: data_word_t;
  
  signal victimRegWrEn			: std_logic;
  signal victimRegDataIn		: data_block_t;
  signal victimRegSet			: std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal victimRegDirty			: std_logic;
  signal victimRegAddr			: std_logic_vector(WORD_ADDR_WIDTH-1 downto 0); 	
  signal victimRegData			: data_block_t;
  
  signal cpuRegReqWord			: std_logic;
  signal cpuReqRegWrEn			: std_logic;
  signal cpuReqRegAddr			: std_logic_vector(WORD_ADDR_WIDTH-1 downto 0); 
  signal cpuReqRegData			: data_block_t;
  
  -- end ADDED internal signal

  -------------------------- README BITCH --------------------------
  -- a bit more tricky than for bus controller 
  -- we gotta implement the 3 components (tristatebuffer, cpureqreg and victumreg) : easy
  -- but we gotta link all that shit with all the muxes and breakers like on the datapath
  -- each input or ouput (in the port list above) is grey on the PDF datapath
  -- we probably have to create an internal signal for each of the input/output of 
  --  our intern component, 
  -- like they did for DataArray and TagArray
  -- then we map then to the components
  -- but how can we link all the mutliplixers and stuff ? if we have to declare an 
  --  internal signal for each output/in of them, this is a shit ton

  --also, the write state machine is more or less coded, with intern signal 
  --  (supposed to be linked to intern components), that doesn't exist yet.
  
  
  --------------------------- REMINDER ------------------------
  -- create the missing signals
  -- map this signal to the component created after TagArray_1 and DataArray_1
  -- implement architecture of CpuReqReg, RdDataTriStateBuffer, VictimReg
  -- implement the state machine
  
  
begin  -- architecture rtl

  comb_proc : process () is
  begin  -- process comb_proc
    -- internal flags default values
    cacheStNext <= cacheSt;
    cpuReqRegWrEn <= '0';
    victimRegWrEn <= '0';
    tagLookupEn <= '0';
    tagWrEn <= '0';
    tagWrSetDirty <= '0';
    dataArrayWrEn <= '0';
    dataArrayWrWord <= '0';
    busOutEn <= '0';
    cacheRdOutEn <= '0';
	
	cacheRdDataIn <= tmpDataArrayRdData;

    -- output default values
    cacheDone <= '0';
    busReq <= '0';

    -- signals with dont care initialization

    -- control: state machine
    case cacheSt is
      when ST_IDLE =>
        if (cacheCs = '1' and cacheWrite = '1') then
          cacheStNext <= ST_WR_HIT_TEST;
          cpuReqRegWrEn <= '1';
          dataArrayAddr <= cacheAddr;
          tagAddr <= cacheAddr;
          tagLookupEn <= '1';
        end if;

      -----------------------------------------------------------------------
      -- rd state machine
      -----------------------------------------------------------------------
      when ST_RD_HIT_TEST =>

      when ST_RD_WAIT_BUS_GRANT_ACC =>

      when ST_RD_WAIT_BUS_COMPLETE_ACC =>

      when ST_RD_WAIT_BUS_GRANT_WB =>

      -----------------------------------------------------------------------
      -- wr state machine
      -----------------------------------------------------------------------
      when ST_WR_HIT_TEST =>
        if (tagHitEn = '1') then 
          cacheStNext <= ST_IDLE;
          cacheDone <= '1';
          tagWrEn <= '1';
          tagWrSet <= tagHitSet;
          tagWrSetDirty <= '1';
          tagAddr <= cpuReqRegAddr;
          dataArrayWrEn <= '1';
          dataArrayWrWord <= '1';
          dataArrayWrSetIdx <= tagHitSet;
          dataArrayWrData <= cpuReqRegData;
        else 
          cacheStNext <= ST_WR_WAIT_BUS_GRANT;
        end if;

      when ST_WR_WAIT_BUS_GRANT =>
        if (busGrant = '1') then
          cacheStNext <= ST_WR_WAIT_BUS_COMPLETE;
          busReq <= '1';
          busOutEn <= '1';
          busCmd <= BUS_WRITE_WORD;
          busAddrIn <= cpuReqRegAddr;
          busDataIn <= cpuReqRegData;
        else
          busReq <= '1';
        end if;

      when ST_WR_WAIT_BUS_COMPLETE =>
        if (busGrant = '0') then
          cacheStNext <= ST_IDLE;
          cacheDone <= '1';
        end if;

      when others => null;
    end case;

    -- datapath:

  end process comb_proc;

  
  busDataWordPick : process(cpuRegReqWord, busData) is
  begin
	busDataWord <= busData(to_integer(unsigned(cpuRegReqWord)));
  end process busDataWordPick;
  
  victimRegDataInPick : process(tagVictimSet, dataArrayRdData) is
  begin
	victimRegDataIn <= dataArrayRdData(to_integer(unsigned(tagVictimSet)));
  end process victimRegDataInPick;
  
  dataArrayRdDataPick : process(tagHitSet, cpuRegReqWord, dataArrayRdData) is 
  begin
	tmpDataArrayRdData <= dataArrayAddr(to_integer(unsigned(tagHitSet)))(to_integer(unsigned(cpuRegReqWord)));
  end process dataArrayRdDataPick;

 -- end ADDED process
 
 -- componenent mapping
 
 TagArray_1 : TagArray
    port map (
      clk            => clk,
      rst            => rst,
      tagLookupEn    => tagLookupEn,
      tagWrEn        => tagWrEn,
      tagWrSetDirty  => tagWrSetDirty,
      tagWrSet       => tagWrSet,
      tagAddr        => tagAddr,
      tagHitEn       => tagHitEn,
      tagHitSet      => tagHitSet,
      tagVictimSet   => tagVictimSet,
      tagVictimDirty => tagVictimDirty,
      tagVictimAddr  => tagVictimAddr);

  DataArray_1 : DataArray
    port map (
      clk               => clk,
      dataArrayWrEn     => dataArrayWrEn,
      dataArrayWrWord   => dataArrayWrWord,
      dataArrayWrSetIdx => dataArrayWrSetIdx,
      dataArrayAddr     => dataArrayAddr,
      dataArrayWrData   => dataArrayWrData,
      dataArrayRdData   => dataArrayRdData);
	  
  -- begin ADDED component
  BusTriStateBufferForCacheController_1 : BusTriStateBufferForCacheController
    port map (
      busOutEn		=> busOutEn,
	  busCmdIn		=> ,-- TODO
      busDataIn		=> ,-- TODO
	  busAddrIn 	=> ,-- TODO
	  busCmd 		=> busCmd,
      busData		=> busData,
	  busAddr		=> busAddr);
	  
  VictimReg_1 : VictimReg
	port map (
	 victimRegWrEn		=> victimRegWrEn,
	 victimRegSetIn		=> tagVictimSet,
	 victimRegDirtyIn	=> tagVictimDirty,
	 victimRegAddrIn	=> tagVictimAddr,
	 victimRegDataIn	=> victimRegDataIn,
	 victimRegSet		=> victimRegSet,
	 victimRegDirty		=> victimRegDirty,
	 victimRegAddr		=> victimRegAddr,
	 victimRegData		=> victimRegData);
	
  CpuReqReg_1 : CpuReqReg
	port map (
	 cpuReqRegWrEn		=> cpuReqRegEn,
	 cpuReqRegAddrIn	=> cacheAddr,
	 cpuReqRegDataIn	=> cacheWrData,
	 cpuReqRegAddr		=> cpuReqRegAddr,
	 cpuReqRegWord		=> cpuRegReqWord,
	 cpuReqRegData		=> cpuReqRegData
	);

  RdDataTriStateBuffer_1 : RdDataTriStateBuffer
	port map (
	 cacheRdOutEn		=> cacheRdOutEn,
	 cacheRdDataIn		=> cacheRdDataIn,
	 cacheRdData		=> cacheRdData);
	  
	  
	  
  -- end ADDED component

  clk_proc : process (clk, rst) is
  begin  -- process clk_proc
    if rst = '0' then                   -- asynchronous reset (active low)
      cacheSt <= ST_IDLE;
    elsif clk'event and clk = '1' then  -- rising clock edge
      cacheSt <= cacheStNext;

      -- there should be more stuff here
  end process clk_proc;

end architecture rtl;


library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity BusTriStateBufferForCacheController is

  port (
    busOutEn  : in  std_logic;
    busCmdIn  : in  bus_cmd_t;
    busDataIn : in  data_block_t;
	busAddrIn : in  std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	busCmd 	  : out bus_cmd_t;
	busData   : out data_block_t;
	busAddr   : out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0)
	);

end entity BusTriStateBufferForCacheController;

architecture tsb of BusTriStateBufferForCacheController is

begin
  busData		<= busDataIn when (busOutEn = '1') else DATA_BLOCK_HIGH_IMPEDANCE;
	busCmd  	<= busCmdIn  when (busOutEn = '1') else (others => 'Z');
  busAddrIn	<= busAddr when (busOutEn = '1') else (others => 'Z');
end architecture tsb;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity CpuReqReg is

  port (
	cpuReqRegWrEn	:	in std_logic;
	cpuReqRegAddrIn	:	in std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	cpuReqRegDataIn	:	in data_word_t;
	cpuReqRegAddr	:	out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	cpuReqRegWord	:	out std_logic;
	cpuReqRegData	: 	out data_word_t);

end entity CpuReqReg;

architecture crr of CpuReqReg is

begin
  if (cpuReqRegWrEn = '1') then 
    cpuReqRegAddr <= cpuReqRegAddrIn;
    cpuReqRegData <= cpuReqRegDataIn;
    cpuReqRegWord <= cpuReqRegDataIn(getBlockIdx(cpuReqRegAddrIn));
  end if;

end architecture crr;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity VictimReg is

  port (
	victimRegWrEn	: in std_logic;
	victimRegSetIn	: in std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegDirtyIn: in std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegAddrIn	: in std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	victimRegDataIn	: in data_word_t;
	victimRegSet	: out std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegDirty	: out std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegAddr	: out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	victimRegData	: out data_word_t);

end entity VictimReg;

architecture vr of VictimReg is

begin
  if (victimRegWrEn = '1') then 
    victimRegSet <= victimRegSetIn;
    victimRegDirty <= victimRegDirtyIn;
    victimRegAddr <= victimRegAddrIn;
    victimRegData <= victimRegDataIn;
  end if;

end architecture vr;



library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity RdDataTriStateBuffer is

  port (
	cacheRdOutEn	: in std_logic;
	cacheRdDataIn	: in data_block_t;
	cacheRdData		: out data_block_t);

end entity RdDataTriStateBuffer;

architecture rdtsb of RdDataTriStateBuffer is

begin
  cacheRdData   <= cacheRdDataIn when (cacheRdOutEn = '1') else DATA_BLOCK_HIGH_IMPEDANCE;
end architecture rdtsb;

