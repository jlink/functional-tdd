package scoreboard.functional;

import io.reactivex.Observable;
import io.reactivex.ObservableSource;
import io.reactivex.subjects.BehaviorSubject;
import io.reactivex.subjects.ReplaySubject;
import io.reactivex.subjects.Subject;
import org.reactivestreams.Publisher;

import java.util.Arrays;
import java.util.stream.Stream;

public class FScoreboardApp {
	public static Observable<String> process(FScoreboard scoreboard, ObservableSource<String> lines) {
		Subject<String> messages = ReplaySubject.create();
		messages.onNext("000:000");
		messages.onComplete();
		return messages;
	}
}
