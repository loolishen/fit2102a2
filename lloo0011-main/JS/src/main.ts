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

// (AI-Assisted) Part C: Added save: false to match the State type
const initialState: State = {
    model: "",
    run: false,
    save: false,
};

const initialOutput: Output = {
    result: "",
    error: "",
};

// (AI-Assisted) Part C: Reset both run and save flags when user types
const modelInput$: Observable<Action<State>> = fromEvent(
    modelInput,
    "input",
).pipe(
    map(event => (event.target as HTMLTextAreaElement).value),
    map(value => state => ({
        ...state,
        model: value,
        run: false,
        save: false,
    })),
);

// (AI-Assisted) Part C: Trigger run and explicitly clear the save flag
const run$: Observable<Action<State>> = fromEvent(runButton, "click").pipe(
    map(() => state => ({
        ...state,
        run: true,
        save: false,
    })),
);

// (AI-Assisted) Part C: Observable stream triggered when Save is clicked
const save$: Observable<Action<State>> = fromEvent(saveButton, "click").pipe(
    map(() => state => ({
        ...state,
        run: false,
        save: true,
    })),
);

// (AI-Assisted) Part C: Send model and whether save was requested to backend
function getOutput(state: State): Observable<Action<Output>> {
    const body = new URLSearchParams();
    body.set("model", state.model);
    body.set("save", state.save ? "true" : "false");

    return fromFetch<Readonly<Record<string, string>>>(
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
            // Pretty-print JSON formatted across multiple lines
            return () => ({ result: JSON.stringify(res, null, 2), error: "" });
        }),
    );
}

function main() {
    // Merge all user input streams into the single state pipeline
    const state$ = merge(modelInput$, run$, save$).pipe(
        scan((state, action) => action(state), initialState),
        startWith(initialState),
    );
    state$.subscribe(console.log);

    // Only make an API call when run or save is active
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