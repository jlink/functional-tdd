package scoreboard.functional;

import static java.util.stream.Collectors.toList;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

public class FunctionalScoreboardTests {

	@Test
	void initialScoreShouldBe000to000() {
		List<String> lines = new ArrayList<>();
		Stream<String> messageStream = FScoreboardApp.process(new FScoreboard(), lines.stream());
		List<String> messages = messageStream.collect(toList());
		assertEquals(1, messages.size());
		assertEquals("000:000", messages.get(0));
	}
}
