package scoreboard.classic;

import net.jqwik.api.*;
import net.jqwik.properties.*;

public class ScoreboardProperties {

	Scoreboard scoreboard = new Scoreboard();

	@Property
	boolean decrementingIsAlwaysPossible(@ForAll Scoreboard scoreboard) {
		scoreboard.decrement();
		return scoreboard.scoreTeamA() >= 0 && scoreboard.scoreTeamB() >= 0;
	}

	@Provide
	Arbitrary<Scoreboard> scoreboards() {
		Arbitrary<Integer> scoreA = Arbitraries.integer().filter(a -> a >= 0);
		Arbitrary<Integer> scoreB = Arbitraries.integer().filter(a -> a >= 0);
		Arbitrary<TeamSelection> teamSelection = Arbitraries.of(TeamSelection.class);

		return Combinators.combine(scoreA, scoreB, teamSelection).as((a, b, t) -> new Scoreboard(a, b, t));
	}
}
