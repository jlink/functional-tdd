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

	@Generate
	Arbitrary<Scoreboard> scoreboards() {
		Arbitrary<Integer> scoreA = Generator.integer().filter(a -> a >= 0);
		Arbitrary<Integer> scoreB = Generator.integer().filter(a -> a >= 0);
		Arbitrary<TeamSelection> teamSelection = Generator.of(TeamSelection.class);

		return Generator.combine(scoreA, scoreB, teamSelection).as((a, b, t) -> new Scoreboard(a, b, t));
	}
}
