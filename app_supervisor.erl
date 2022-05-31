-module(app_supervisor).
-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
   {ok, Pid1} = supervisor:start_link(app_supervisor, []),
   unlink(Pid1).
   %%{ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
   %%{ok, Pid}.


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
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [{mnesia_manager, {mnesia_manager, start_server, []}, permanent, brutal_kill, worker, [mnesia_manager]},
          		  {auctions_manager, {auctions_manager, start_server, []}, permanent, brutal_kill, worker, [auctions_manager]}],
    {ok, {SupFlags, ChildSpecs}}.