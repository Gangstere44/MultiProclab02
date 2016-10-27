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

  signal cacheStNext, cacheSt     : cache_ctrl_state_t := ST_IDLE;

---------------------------- Origin internal signals ---------------------------

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

---------------------------- Custom internal signals ---------------------------

  -- The two signals out of the "made by hand" muxes
  signal busDataWord 			  : data_word_t;
  signal cacheRdDataInAux   : data_word_t;

  -- VictimReg input/output
  signal victimRegWrEn			: std_logic;
  signal victimRegSetIn			: std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal victimRegDirtyIn		: std_logic;
  signal victimRegAddrIn		: std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal victimRegDataIn		: data_block_t;

  signal victimRegSet			  : std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
  signal victimRegDirty			: std_logic;
  signal victimRegAddr			: std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal victimRegData			: data_block_t;

  -- cpuRegReq input/output
  signal cpuReqRegWrEn			: std_logic;
  signal cpuReqRegAddrIn		: std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal cpuReqRegDataIn		: data_block_t;

  signal cpuReqRegAddr			: std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
  signal cpuRegReqWord			: std_logic;
  signal cpuReqRegData			: data_block_t;

  -- BusTriStateBuffer input (no outputs because mapped to the cache outputs)
  signal busOutEn           : std_logic;
  signal busCmdIn           : bus_cmd_t;
  signal busDataIn          : data_block_t;
  signal busAddrIn          : std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);

  -- RdDataTriStateBuffer input (no outputs because mapped to the cache outputs)
  signal cacheRdOutEn       : std_logic;
  signal cacheRdDataIn      : data_word_t;

---------------------------- State machine process -----------------------------

begin

  comb_proc : process () is
  begin
    -- Internal flags default values
    cacheStNext     <= cacheSt;
    cpuReqRegWrEn   <= '0';
    victimRegWrEn   <= '0';
    tagLookupEn     <= '0';
    tagWrEn         <= '0';
    tagWrSetDirty   <= '0';
    dataArrayWrEn   <= '0';
    dataArrayWrWord <= '0';
    busOutEn        <= '0';
    cacheRdOutEn    <= '0';

    -- Output default values
    cacheDone       <= '0';
    busReq          <= '0';


    case cacheSt is
      when ST_IDLE =>
        if (cacheCs = '1' and cacheWrite = '1') then
          cacheStNext   <= ST_WR_HIT_TEST;
          cpuReqRegWrEn <= '1';
          dataArrayAddr <= cacheAddr;
          tagAddr       <= cacheAddr;
          tagLookupEn   <= '1';
        elsif (cacheCs = '1' and cacheRead = '1') then
          cacheStNext   <= ST_RD_HIT_TEST;
          cpuReqRegWrEn <= '1';
          dataArrayAddr <= cacheAddr;
          tagAddr       <= cacheAddr;
          tagLookupEn   <= '1';
        end if;

      -----------------------------------------------------------------------
      -- Read state machine
      -----------------------------------------------------------------------
      when ST_RD_HIT_TEST =>
        if (tagHitEn = '1') then
          cacheStNext   <= ST_IDLE;
          cacheDone     <= '1';
          cacheRdOutEn  <= '1';
          cacheRdData   <= dataArrayRdData(tagHitSet)(cpuRegReqWord) -- TODO
        else
          cacheStNext   <= ST_RD_WAIT_BUS_GRANT_ACC;
          victimRegWrEn <= '1';
        end if;

      when ST_RD_WAIT_BUS_GRANT_ACC =>
        if (busGrant = '1') then
          cacheStNext <= ST_RD_WAIT_BUS_COMPLETE_ACC;
          busReq      <= '1';
          busOutEn    <= '1';
          busCmd      <= BUS_READ;
          busAddrIn   <= cpuReqRegAddr;
        end if;

      when ST_RD_WAIT_BUS_COMPLETE_ACC =>
        if (busGrant = '0' and victimRegDirty = '0') then
          cacheStNext       <= ST_IDLE;
          cacheDone         <= '1';
          cacheRdOutEn      <= '1';
          cacheRdData       <= busDataWord;
          tagWrEn           <= '1';
          tagWrSet          <= victimRegSet;
          tagWrSetDirty     <= '0';
          tagAddr           <= cpuReqRegAddr;
          dataArrayWrEn     <= '1';
          dataArrayWrSetIdx <= victimRegSet;
          dataArrayWrWord   <= '0';
          dataArrayWrData   <= busData;
        elsif (busGrant = '0' and victimRegDirty = '1') then
          cacheStNext       <= ST_RD_WAIT_BUS_GRANT_WB;
          tagWrEn           <= '1';
          tagWrSet          <= victimRegSet;
          tagWrSetDirty     <= '0';
          tagAddr           <= cpuReqRegAddr;
          dataArrayWrEn     <= '1';
          dataArrayWrSetIdx <= victimRegSet;
          dataArrayWrWord   <= '0';
          dataArrayWrData   <= busData;
        end if;

      when ST_RD_WAIT_BUS_GRANT_WB =>
        if (busGrant = '1') then
          cacheStNext <= ST_RD_WAIT_BUS_COMPLETE_WB;
          busReq      <= '1';
          busOutEn    <= '1';
          busCmdIn    <= BUS_WRITE;
          busAddrIn   <= victimRegAddr;
          busDataIn   <= victimRegData;
        else
          busReq      <= '1';
        end if;

      when ST_RD_WAIT_BUS_COMPLETE_WB =>
        if (busGrant = '1') then
          dataArrayAddr <= cpuReqRegAddr;
        else
          cacheStNext   <= ST_IDLE;
          cacheDone     <= '1';
          cacheRdOutEn  <= '1';
          cacheRdData   <= dataArrayRdData(tagHitSet)(cpuReqRegWord) -- TODO
        end if;


      -----------------------------------------------------------------------
      -- Write state machine
      -----------------------------------------------------------------------
      when ST_WR_HIT_TEST =>
        if (tagHitEn = '1') then
          cacheStNext       <= ST_IDLE;
          cacheDone         <= '1';
          tagWrEn           <= '1';
          tagWrSet          <= tagHitSet;
          tagWrSetDirty     <= '1';
          tagAddr           <= cpuReqRegAddr;
          dataArrayWrEn     <= '1';
          dataArrayWrWord   <= '1';
          dataArrayWrSetIdx <= tagHitSet;
          dataArrayWrData   <= cpuReqRegData;
        else
          cacheStNext <= ST_WR_WAIT_BUS_GRANT;
        end if;

      when ST_WR_WAIT_BUS_GRANT =>
        if (busGrant = '1') then
          cacheStNext <= ST_WR_WAIT_BUS_COMPLETE;
          busReq      <= '1';
          busOutEn    <= '1';
          busCmd      <= BUS_WRITE_WORD;
          busAddrIn   <= cpuReqRegAddr;
          busDataIn   <= cpuReqRegData;
        else
          busReq      <= '1';
        end if;

      when ST_WR_WAIT_BUS_COMPLETE =>
        if (busGrant = '0') then
          cacheStNext <= ST_IDLE;
          cacheDone   <= '1';
        end if;

      when others => null;
    end case;

    -- datapath:

  end process comb_proc;

----------------------------- Componenents mapping -----------------------------

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

  BusTriStateBufferForCacheController_1 : BusTriStateBufferForCacheController
    port map (
      busOutEn		=> busOutEn,
      busCmdIn		=> busCmdIn,
      busDataIn		=> busDataIn,
	    busAddrIn 	=> busAddrIn,
	    busCmd 		  => busCmd,
      busData		  => busData,
	    busAddr		  => busAddr);

  VictimReg_1 : VictimReg
	port map (
	 victimRegWrEn		=> victimRegWrEn,
	 victimRegSetIn		=> tagVictimSet,
	 victimRegDirtyIn	=> tagVictimDirtyIn,
	 victimRegAddrIn	=> tagVictimAddrIn,
	 victimRegDataIn	=> victimRegDataIn,
	 victimRegSet		  => victimRegSet,
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
	 cacheRdDataIn	=> cacheRdDataIn,
	 cacheRdData		=> cacheRdData);

-------------------------------- Clock process ---------------------------------

  clk_proc : process (clk, rst) is
  begin  -- process clk_proc
    if rst = '0' then                   -- asynchronous reset (active low)
      cacheSt <= ST_IDLE;
    elsif clk'event and clk = '1' then  -- rising clock edge
      cacheSt <= cacheStNext;
    end if;
      -- there should be more stuff here
  end process;

end architecture;

--------------------- BusTriStateBufferForCacheController ----------------------

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

----------------------------------- CpuReqReg ----------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity CpuReqReg is

  port (
  clk : in std_logic;
  rst : in std_logic;
	cpuReqRegWrEn	:	in std_logic;
	cpuReqRegAddrIn	:	in std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	cpuReqRegDataIn	:	in data_word_t;
	cpuReqRegAddr	:	out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	cpuReqRegWord	:	out std_logic;
	cpuReqRegData	: 	out data_word_t);

end entity CpuReqReg;

architecture crr of CpuReqReg is

begin

  process(clk, rst)
  begin
    if (rst = '1') then
      cpuReqRegAddr <= (others => 0);
      cpuReqRegData <= (others => 0);
      cpuReqRegWord <= (others => 0);
    elsif rising_edge(clk) then
      if (cpuReqRegWrEn = '1') then
        cpuReqRegAddr <= cpuReqRegAddrIn;
        cpuReqRegData <= cpuReqRegDataIn;
        cpuReqRegWord <= cpuReqRegDataIn(getBlockIdx(cpuReqRegAddrIn));
      end if;
    end if;
  end process;

end architecture;

----------------------------------- VictimReg ----------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.mem_types.all;
use work.mem_components.all;

entity VictimReg is

  port (
  clk              : in std_logic;
  rst              : in std_logic;
	victimRegWrEn	   : in std_logic;
	victimRegSetIn	 : in std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegDirtyIn : in std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegAddrIn	 : in std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	victimRegDataIn	 : in data_word_t;
	victimRegSet	   : out std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegDirty	 : out std_logic_vector(SET_ADDR_WIDTH-1 downto 0);
	victimRegAddr	   : out std_logic_vector(WORD_ADDR_WIDTH-1 downto 0);
	victimRegData	   : out data_word_t);

end entity VictimReg;

architecture vr of VictimReg is

begin

  process(clk, rst)
  begin
    if (rst = '1') then
      victimRegSet    <= (others => '0');
      victimRegDirty  <= (others => '0');
      victimRegAddr   <= (others => '0');
      victimRegData   <= (others => '0');
    elsif rising_edge(clk) then
      if (victimRegWrEn = '1') then
        victimRegSet    <= victimRegSetIn;
        victimRegDirty  <= victimRegDirtyIn;
        victimRegAddr   <= victimRegAddrIn;
        victimRegData   <= victimRegDataIn;
      end if;
    end if;
  end process;

end architecture;

----------------------------- RdDataTriStateBuffer -----------------------------

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
