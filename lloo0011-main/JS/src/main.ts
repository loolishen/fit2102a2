import {
    fromEvent,
    map,
    merge,
    scan,
    startWith,
    switchMap,
    filter,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import { Action, Output, State } from "./types";

const modelInput = document.getElementById(
    "model-input",
) as HTMLTextAreaElement;

const runButton = document.getElementById("run-model")!;

const saveButton = document.getElementById("save-model")!;

const outputEl = document.getElementById("model-output") as HTMLOutputElement;

const initialState: State = {
    model: "",
    run: false,
};

const initialOutput: Output = {
    result: "",
    error: "",
};

const modelInput$: Observable<Action<State>> = fromEvent(
    modelInput,
    "input",
).pipe(
    map(event => (event.target as HTMLTextAreaElement).value),
    map(value => state => ({
        ...state,
        model: value,
        run: false,
    })),
);

const run$: Observable<Action<State>> = fromEvent(runButton, "click").pipe(
    map(() => state => ({
        ...state,
        run: true,
    })),
);

// observable stream triggered when Save is clicked
const save$: Observable<Action<State>> = fromEvent(saveButton, "click").pipe(
    map(() => state => ({
        ...state,
        run: false,
        save: true,
    })),
);

// send model and whether save was requested to backend
function getOutput(state: State): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("model", state.model);
    if (state.save) {
        body.set("save", "true");
    }

    return fromFetch<Readonly<{ result: string } | { error: string }>>(
        "/api/minizinc",
        {
            method: "POST",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            body: body.toString(),
            selector: res => res.json(),
        },
    ).pipe(
        map(res => {
            if ("error" in res) {
                return () => ({ result: "", error: res.error });
            }
            return () => ({ result: JSON.stringify(res), error: "" });
        }),
    );
}

function getOutput(state: State): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("model", state.model);
    // (AI-Assisted) Part C: Signal to Haskell whether to save the solution
    body.set("save", state.save ? "true" : "false");

    return fromFetch<Readonly<{ result: string } | { error: string }>>(
        "/api/minizinc",
        {
            method: "POST",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            body: body.toString(),
            selector: res => res.json(),
        },
    ).pipe(
        map(res => {
            if ("error" in res) {
                return () => ({ result: "", error: res.error });
            }
            return () => ({ result: JSON.stringify(res), error: "" });
        }),
    );
}

function main() {
    const state$ = merge(modelInput$, run$, save$).pipe(
        scan((state, action) => action(state), initialState),
        startWith(initialState),
    );
    state$.subscribe(console.log);

    const output$ = state$.pipe(
        filter(state => state.run || state.save),
        switchMap(getOutput),
        scan((output, action) => action(output), initialOutput),
        startWith(initialOutput),
    );

    output$.subscribe(output => {
        console.log(output);
        outputEl.value = output.error || output.result;
    });
}

if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}