package scoreboard.classic;

import javaslang.test.Arbitrary;
import javaslang.test.CheckResult;
import javaslang.test.Gen;
import javaslang.test.Property;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class ScoreboardTests {

    Scoreboard scoreboard = new Scoreboard();

    @Test
    void initialScoreIs000to000() {
        assertScore(0, 0);
    }

    private void assertScore(int expectedA, int expectedB) {
        assertEquals(expectedA, scoreboard.scoreTeamA(), "Team A Score");
        assertEquals(expectedB, scoreboard.scoreTeamB(), "Team B Score");
    }

    @Test
    void initiallyNoTeamIsSelected() {
        assertFalse(scoreboard.isTeamASelected());
        assertFalse(scoreboard.isTeamBSelected());
    }

    @Test
    void selectingTeamAMakesItSelected() {
        scoreboard.selectTeamA();
        assertTrue(scoreboard.isTeamASelected());
    }

    @Test
    void selectingTeamBMakesItSelected() {
        scoreboard.selectTeamB();
        assertTrue(scoreboard.isTeamBSelected());
    }

    @Test
    void lastSelectCallIsRelevant() {
        scoreboard.selectTeamB();
        scoreboard.selectTeamA();
        assertTrue(scoreboard.isTeamASelected());
        assertFalse(scoreboard.isTeamBSelected());
        scoreboard.selectTeamA();
        assertTrue(scoreboard.isTeamASelected());
        assertFalse(scoreboard.isTeamBSelected());
        scoreboard.selectTeamB();
        assertTrue(scoreboard.isTeamBSelected());
        assertFalse(scoreboard.isTeamASelected());
    }

    @Test
    void incrementIncrementsScoreOfSelectedTeam() {
        scoreboard.setScore(1, 2);
        scoreboard.selectTeamA();
        scoreboard.increment();
        assertScore(2, 2);
        assertTrue(scoreboard.isTeamASelected());

        scoreboard.setScore(1, 2);
        scoreboard.selectTeamB();
        scoreboard.increment();
        assertScore(1, 3);
        assertTrue(scoreboard.isTeamBSelected());
    }

    @Test
    void decrementDecrementsScoreOfSelectedTeam() {
        scoreboard.setScore(10, 10);
        scoreboard.selectTeamA();
        scoreboard.decrement();
        assertScore(9, 10);
        assertTrue(scoreboard.isTeamASelected());

        scoreboard.setScore(10, 10);
        scoreboard.selectTeamB();
        scoreboard.decrement();
        assertScore(10, 9);
    }

    @Test
    void whenNoTeamIsSelectedIncrementAndDecrementLeaveScoreAsIs() {
        scoreboard.setScore(10, 10);
        scoreboard.increment();
        assertScore(10, 10);
        scoreboard.decrement();
        assertScore(10, 10);
    }

    @Test
    void resetScoreSetsScoreTo0to0() {
        scoreboard.setScore(10, 10);
        scoreboard.resetScore();
        assertScore(0, 0);
    }

    @Test
    void noTeamSelectedAfterReset() {
        scoreboard.selectTeamA();
        scoreboard.resetScore();
        assertFalse(scoreboard.isTeamASelected());
        assertFalse(scoreboard.isTeamBSelected());
    }

    @Test
    void decrementingIsAlwaysPossible() {

        CheckResult property = Property.def("decrementing is always possible")
                .forAll(scoreboards())
                .suchThat(scoreboard -> {
                    scoreboard.decrement();
                    return scoreboard.scoreTeamA() >= 0 && scoreboard.scoreTeamB() >= 0;
                }).check();

        property.assertIsSatisfied();
    }

    private Arbitrary<Scoreboard> scoreboards() {
        Arbitrary<Integer> scoreA = Arbitrary.integer().filter(a -> a >= 0);
        Arbitrary<Integer> scoreB = Arbitrary.integer().filter(a -> a >= 0);
        Arbitrary<TeamSelection> teamSelection = size -> Gen.choose(TeamSelection.values());

        return size -> random -> {
            int a = scoreA.apply(size).apply(random);
            int b = scoreB.apply(size).apply(random);
            TeamSelection t = teamSelection.apply(size).apply(random);
            return new Scoreboard(a, b, t);
        };
    }
}
