%distance(0, 0, 0).  % a dummy predicate to make the sim work.
% state(StateId, Agents, CurrentTurn, TurnOrder).
% history(StateId, UniverseId, Time, Turn).

distance(Agent, TargetAgent, Distance) :- 
		Agent.x = X1, Agent.y = Y1, TargetAgent.x = X2, TargetAgent.y = Y2, Distance is abs(X1-X2) + abs(Y1-Y2).


% if else düzelt
multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance) :-
	state(StateId,Agents1,_,_),  Agent = Agents1.get(AgentId), history(StateId,U1,T1,_), Agent.x = X1, Agent.y = Y1, Class = Agent.class,
	state(TargetStateId, Agents2, _, _), TargetAgent = Agents2.get(TargetAgentId), history(TargetStateId,U2,T2,_), TargetAgent.x = X2, TargetAgent.y = Y2,
	(Class = wizard -> Distance is (abs(X1 - X2) + abs(Y1-Y2) + 2 * (abs(T1-T2) + abs(U1-U2))); Distance is (abs(X1- X2) + abs(Y1-Y2) + 5 * (abs(T1-T2) + abs(U1-U2)))).

	%(
	%(Class = wizard -> Distance is (abs(X1 - X2) + abs(Y1-Y2) + 2 * (abs(T1-T2) + abs(U1-U2)))); 
	%(Class = rogue -> Distance is (abs(X1- X2) + abs(Y1-Y2) + 5 * (abs(T1-T2) + abs(U1-U2))));
	%(Class = warrior -> Distance is (abs(X1- X2) + abs(Y1-Y2) + 5 * (abs(T1-T2) + abs(U1-U2))))
	%).


find_min_list([X], X).
find_min_list([H | T], Min) :- 
    find_min_list(T, Min2),
    Min is min(H,Min2), !.


% to look for if the key is included in the dictionary
dict_member(Key, Dict) :-
	findall(Keys-Values, Values = Dict.get(Keys) , AllPairs),
	%dict_pairs(Dict, _, AllPairs), %AllPairs is the all key-value pairs in key-value style
	member(Key- _, AllPairs).

% Finds minimum element in a list of lists with 2 elements (i.e. [[a,b], [c,d]])
find_min(List, NearestAgentId, Min) :-
	findall(Distance, member((NearestAgentId,Distance), List), Distances), find_min_list(Distances, Min), member((NearestAgentId,Min), List).

%finds all enemy agents and their distances and finds their minimum distance
nearest_agent(StateId, AgentId, NearestAgentId, Distance) :-
	state(StateId, Agents, _, _), Agent = Agents.get(AgentId), findall(TargetAgent, dict_member(TargetAgent,Agents) ,AllAgents),
	findall((NearestAgent, Dist), (member(NearestAgent, AllAgents), TargetAgent = Agents.get(NearestAgent), TargetAgent.name \= Agent.name ,distance(Agent, TargetAgent, Dist)), AllDistances),
	findall(D, member((NearestAgentId,D), AllDistances), Distances), find_min_list(Distances, Distance), member((NearestAgentId,Distance), AllDistances), !.


% Outer findall predicate finds all the states and each agent's distance to AgentId in StateId
% in (TargetState, TargetAgent, ItsDistance) fashion.
find_distances_in_all_states(StateId,AgentId,AllStates) :-
	state(StateId, Agents, _, _), Agent = Agents.get(AgentId), 
	findall((TargetState, TargetAgent, ItsDistance), (history(TargetState, _, _, _), state(TargetState, TargetAgents, _,_), dict_member(TargetAgent, TargetAgents), Enemy = TargetAgents.get(TargetAgent), Enemy.name \= Agent.name ,multiverse_distance(StateId, AgentId, TargetState, TargetAgent, ItsDistance)), AllStates).


nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance) :- 
	state(StateId, Agents, _, _), Agent = Agents.get(AgentId), find_distances_in_all_states(StateId, AgentId,AllStates),
	findall(Dist, member((TargetStateId, TargetAgentId, Dist) , AllStates), Distances), find_min_list(Distances, Distance), member((TargetStateId, TargetAgentId, Distance) , AllStates), !.
	
find_length([],0).
find_length([_ | T], Length) :-
    find_length(T, Length2),
    Length is Length2 + 1.

%listlerin lengthi bulmak lazım
num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues) :-
	state(StateId, Agents, _,_), findall(WarriorId, (dict_member(WarriorId, Agents), Warrior = Agents.get(WarriorId)  ,Warrior.class = warrior, Warrior.name \= Name) , Warriors),
	findall(WizardId, (dict_member(WizardId, Agents), Wizard = Agents.get(WizardId)  ,Wizard.class = wizard, Wizard.name \= Name) , Wizards),
	findall(RogueId, (dict_member(RogueId, Agents), Rogue = Agents.get(RogueId)  ,Rogue.class = rogue, Rogue.name \= Name) , Rogues),
	find_length(Rogues, NumRogues), find_length(Wizards, NumWizards), find_length(Warriors, NumWarriors).


difficulty_of_state(StateId, Name, AgentClass, Difficulty) :-
	num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues), 
	(
	(AgentClass = warrior -> Difficulty is 5 * NumWarriors + 8 * NumWizards + 2 * NumRogues);
	(AgentClass = wizard -> Difficulty is 2 * NumWarriors + 5 * NumWizards + 8 * NumRogues);
	(AgentClass = rogue -> Difficulty is 8 * NumWarriors + 2 * NumWizards + 5 * NumRogues)
	).

% distance(Agent, TargetAgent, Distance).
% multiverse_distance(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
% nearest_agent(StateId, AgentId, NearestAgentId, Distance).
% nearest_agent_in_multiverse(StateId, AgentId, TargetStateId, TargetAgentId, Distance).
% num_agents_in_state(StateId, Name, NumWarriors, NumWizards, NumRogues).
% difficulty_of_state(StateId, Name, AgentClass, Difficulty).
% easiest_traversable_state(StateId, AgentId, TargetStateId).
% basic_action_policy(StateId, AgentId, Action).
