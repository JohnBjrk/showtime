import {
  StartTestRun,
  StartTestSuite,
  TestModule,
  EndTestRun,
  EndTest,
  TestFunction,
  StartTest,
  EndTestSuite,
} from "./showtime/internal/common/test_suite.mjs";
import {
  Assert,
  ErlangException,
  GenericException,
  GleamAssert,
  GleamError,
  ErlangError,
  TraceList,
  TestFunctionReturn,
  Ignored,
  Ignore,
  TraceModule,
  Num,
} from "./showtime/internal/common/test_result.mjs";
import { Eq, NotEq, IsOk, IsError, Fail } from "./showtime/tests/should.mjs";
import { Error, List, Ok } from "./gleam.mjs";
import { None, is_some, unwrap } from "../gleam_stdlib/gleam/option.mjs";
let util;
if (!globalThis.Deno) {
  util = await import("node:util");
}
export const run = async (
  eventHandler,
  init_state,
  module_list,
  ignore_list
) => {
  const moduleListArray = is_some(module_list)
    ? unwrap(module_list, List.fromArray([])).toArray()
    : undefined;
  const ignoreListArray = ignore_list.toArray();
  let state = init_state;
  state = eventHandler(new StartTestRun(), state);

  let num_modules = 0;

  let packageName = await readRootPackageName();
  let dist = `../${packageName}/`;

  const originalConsoleLog = console.log;
  let outputCapture = new OutputCapture();
  let outputBuffer = [];

  for await (let path of await gleamFiles("test")) {
    let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
    const module_name = js_path.split(".")[0];
    const test_module = new TestModule(module_name, path);
    state = eventHandler(new StartTestSuite(test_module), state);
    let module = await import(join_path(dist, js_path));
    if (moduleListArray && !moduleListArray.includes(module_name)) continue;
    for (let fnName of Object.keys(module)) {
      if (!fnName.endsWith("_test")) continue;
      state = eventHandler(
        new StartTest(test_module, new TestFunction(fnName)),
        state
      );
      try {
        console.log = (message, ...args) => {
          outputCapture.captureOutput(message, ...args);
        };
        let result = await module[fnName]();
        console.log = originalConsoleLog;
        outputBuffer = outputCapture.getCapturedOutput();
        outputCapture = new OutputCapture();
        if (result && result.test_function) {
          if (
            result.meta.tags
              .toArray()
              .some((tag) => ignoreListArray.includes(tag))
          ) {
            console.log(`Ignoring: ${fnName}`);
            state = eventHandler(
              new EndTest(
                test_module,
                new TestFunction(fnName),
                new Ok(new Ignored(new Ignore()))
              ),
              state
            );
            continue;
          } else {
            console.log = (message, ...args) => {
              outputCapture.captureOutput(message, ...args);
            };
            result = result.test_function();
            console.log = originalConsoleLog;
            outputBuffer = outputCapture.getCapturedOutput();
            outputCapture = new OutputCapture();
          }
        }
        state = eventHandler(
          new EndTest(
            test_module,
            new TestFunction(fnName),
            new Ok(
              new TestFunctionReturn(
                result,
                List.fromArray(outputBuffer.reverse())
              )
            )
          ),
          state
        );
      } catch (error) {
        console.log = originalConsoleLog;
        outputBuffer = outputCapture.getCapturedOutput();
        let stacktrace = [];
        if (error.stack) {
          stacktrace = parseStacktrace(error.stack);
        }
        let moduleName = "\n" + js_path.slice(0, -4);
        if (
          error &&
          error.value &&
          error.value instanceof Error &&
          (error.value[0] instanceof Eq ||
            error.value[0] instanceof NotEq ||
            error.value[0] instanceof IsOk ||
            error.value[0] instanceof IsError ||
            error.value[0] instanceof Fail)
        ) {
          state = eventHandler(
            new EndTest(
              test_module,
              new TestFunction(fnName),
              new Error(
                new ErlangException(
                  new ErlangError(),
                  new GleamError(
                    new Assert(
                      test_module.name,
                      fnName,
                      // TODO: Fix line number
                      error.line ? error.line : 0,
                      "DUMMY_MESSAGE",
                      error.value
                    )
                  ),
                  new TraceList(List.fromArray(stacktrace)),
                  List.fromArray(outputBuffer.reverse())
                )
              )
            ),
            state
          );
        } else if (error && error.message && error.message.includes("should")) {
          const error_parts = error.message.split("\n");
          const got = error_parts[1]
            ? error_parts[1].trim()
            : "COULD NOT PARSE GOT FROM EXCEPTION";
          const assertion = error_parts[2]
            ? error_parts[2].trim()
            : "COULD NOT PARSE ASSERTION FROM EXCEPTION";
          const expected = error_parts[3]
            ? error_parts[3].trim()
            : "COULD NOT PARSE EXPECTED FROM EXCEPTION";
          let result = new Eq(got, expected, new None());
          if (assertion === "should equal") {
            result = new Eq(got, expected, new None());
          } else if (assertion === "should not equal") {
            result = new NotEq(got, expected, new None());
          } else if (assertion === "should be ok") {
            result = new IsOk(
              new Error(got.substring(6, got.length - 1)),
              new None()
            );
          } else if (assertion === "should be error") {
            result = new IsError(
              new Ok(got.substring(3, got.length - 1)),
              new None()
            );
          }
          state = eventHandler(
            new EndTest(
              test_module,
              new TestFunction(fnName),
              new Error(
                new ErlangException(
                  new ErlangError(),
                  new GleamError(
                    new Assert(
                      test_module.name,
                      fnName,
                      error.line ? error.line : 0,
                      "DUMMY_MESSAGE", // TODO: Base this on the middle word (between expected and got)
                      new Error(result)
                    )
                  ),
                  new TraceList(List.fromArray(stacktrace)),
                  List.fromArray(outputBuffer.reverse())
                )
              )
            ),
            state
          );
        } else if (error && error.gleam_error) {
          state = eventHandler(
            new EndTest(
              test_module,
              new TestFunction(fnName),
              new Error(
                new ErlangException(
                  new ErlangError(),
                  new GleamAssert(error.value, error.line ? error.line : 0),
                  new TraceList(List.fromArray(stacktrace)),
                  List.fromArray(outputBuffer.reverse())
                )
              )
            ),
            state
          );
        } else {
          state = eventHandler(
            new EndTest(
              test_module,
              new TestFunction(fnName),
              new Error(
                new ErlangException(
                  new ErlangError(),
                  new GenericException(error),
                  new TraceList(List.fromArray(stacktrace)),
                  List.fromArray(outputBuffer.reverse())
                )
              )
            ),
            state
          );
        }
        outputCapture = new OutputCapture();
      }
    }
    state = eventHandler(new EndTestSuite(test_module), state);
    num_modules++;
  }
  state = eventHandler(new EndTestRun(num_modules), state);
};

function parseStacktrace(stack) {
  return stack
    .split("\n")
    .filter((line) => line.trim().startsWith("at "))
    .map((line) => {
      const segments = line.trim().split(" ");
      const functionName = segments.length > 2 ? segments[1] : undefined;
      const functionNameSplit = functionName && functionName.split(".");
      const functionNameShort =
        functionNameSplit && functionNameSplit.length > 1
          ? functionNameSplit[1]
          : functionName;
      const modulePart =
        segments.length > 2
          ? segments[2].split(".mjs")
          : segments[1].split(".mjs");
      const moduleName = modulePart[0].split("/").pop();
      return new TraceModule(
        moduleName,
        functionNameShort || "",
        new Num(0),
        []
      );
    })
    .reverse();
}

class OutputCapture {
  constructor() {
    this.outputBuffer = [];
  }

  captureOutput(message, ...args) {
    const line = globalThis.Deno ? message : util.format(message, ...args);
    this.outputBuffer.push(line);
  }

  getCapturedOutput() {
    return this.outputBuffer;
  }
}

async function* gleamFiles(directory) {
  for (let entry of await read_dir(directory)) {
    let path = join_path(directory, entry);
    if (path.endsWith(".gleam")) {
      yield path;
    } else {
      try {
        yield* gleamFiles(path);
      } catch (error) {
        // Could not read directory, assume it's a file
      }
    }
  }
}

async function readRootPackageName() {
  let toml = await read_file("gleam.toml", "utf-8");
  for (let line of toml.split("\n")) {
    let matches = line.match(/\s*name\s*=\s*"([a-z][a-z0-9_]*)"/); // Match regexp in compiler-cli/src/new.rs in validate_name()
    if (matches) return matches[1];
  }
  throw new Error("Could not determine package name from gleam.toml");
}

async function read_dir(path) {
  if (globalThis.Deno) {
    let items = [];
    for await (let item of Deno.readDir(path, { withFileTypes: true })) {
      items.push(item.name);
    }
    return items;
  } else {
    let { readdir } = await import("fs/promises");
    return readdir(path);
  }
}

function join_path(a, b) {
  if (a.endsWith("/")) return a + b;
  return a + "/" + b;
}

async function read_file(path) {
  if (globalThis.Deno) {
    return Deno.readTextFile(path);
  } else {
    let { readFile } = await import("fs/promises");
    let contents = await readFile(path);
    return contents.toString();
  }
}

export function exit(code) {
  if (globalThis.Deno) {
    Deno.exit(code);
  } else {
    process.exit(code);
  }
}

export function system_time() {
  return Date.now();
}

export const start_args = function () {
  if (globalThis.Deno) {
    return List.fromArray(Deno.args);
  } else {
    return List.fromArray(process.argv.slice(1));
  }
};
