package scoreboard.classic;

import net.jqwik.api.*;

public class ScoreboardProperties {

	@Property
	boolean decrementingIsAlwaysPossible(@ForAll Scoreboard scoreboard) {
		scoreboard.decrement();
		return scoreboard.scoreTeamA() >= 0 && scoreboard.scoreTeamB() >= 0;
	}

	@Provide
	Arbitrary<Scoreboard> scoreboards() {
		Arbitrary<Integer> scoreA = Arbitraries.integers().filter(a -> a >= 0);
		Arbitrary<Integer> scoreB = Arbitraries.integers().filter(a -> a >= 0);
		Arbitrary<TeamSelection> teamSelection = Arbitraries.of(TeamSelection.class);

		return Combinators.combine(scoreA, scoreB, teamSelection).as((a, b, t) -> new Scoreboard(a, b, t));
	}
}
