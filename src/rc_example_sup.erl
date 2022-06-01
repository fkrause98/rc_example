%%%-------------------------------------------------------------------
%% @doc rc_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(rc_example_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
  VMaster = {rc_example_vnode_master,
             {riak_core_vnode_master, start_link, [rc_example_vnode]},
             permanent, 5000, worker, [riak_core_vnode_master]},

  CoverageFSM = {rc_example_coverage_fsm_sup,
                 {rc_example_coverage_fsm_sup, start_link, []},
                 permanent, infinity, supervisor, [rc_example_coverage_fsm_sup]},

  {ok, {{one_for_one, 5, 10}, [VMaster, CoverageFSM]}}.
%%
%% internal functions
