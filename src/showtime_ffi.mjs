import { StartTestRun, StartTestSuite, TestModule, EndTestRun, EndTest, TestFunction, StartTest, EndTestSuite } from "./showtime/common/test_suite.mjs";
import { Assert, ErlangException, GleamError, ErlangError, TraceList, TestFunctionReturn, TraceModule, Num } from "./showtime/common/test_result.mjs"
import { Eq } from "./showtime/tests/should.mjs"
import { Error, List, Ok } from "./gleam.mjs"
import { None } from "../gleam_stdlib/gleam/option.mjs";
export const test = async (eventHandler, init_state) => {
    let state = init_state
    state = eventHandler(new StartTestRun(), state)

    let num_modules = 0;

    let packageName = await readRootPackageName();
    let dist = `../${packageName}/`;

    for await (let path of await gleamFiles("test")) {
      let js_path = path.slice("test/".length).replace(".gleam", ".mjs");
      const test_module = new TestModule(js_path, js_path)
      state = eventHandler(new StartTestSuite(test_module), state)
      let module = await import(join_path(dist, js_path));
      for (let fnName of Object.keys(module)) {
        if (!fnName.endsWith("_test")) continue;
        state = eventHandler(new StartTest(test_module, new TestFunction(fnName)), state)
        try {
          let result = await module[fnName]();
          if (result && result.test_function) {
            result = result.test_function()
          }
          state = eventHandler(
            new EndTest(
              test_module, 
              new TestFunction(fnName), 
              new Ok(
                new TestFunctionReturn(result)
              )
            ),
            state
          )
        } catch (error) {
          let stacktrace = []
          if (error.stack) {
            stacktrace = error.stack.split("\n")
              .filter(line => line.trim().startsWith("at "))
              .map(line => {
                const segments = line.trim().split(" ")
                const functionName = segments.length > 2 ? segments[1] : undefined
                const modulePart = segments.length > 2 ? segments[2].split(".mjs") : segments[1].split(".mjs")
                const moduleName = modulePart[0].split("/").pop()
                return new TraceModule(moduleName, functionName || "", new Num(0), [])
              }).reverse()
          }
          let moduleName = "\n" + js_path.slice(0, -4);
          // let line = error.line ? `:${error.line}` : "";
          // write(`\n❌ ${moduleName}.${fnName}${line}: ${error}\n`);
          if (error && error.value) {
            // callback(new StartTestRun(), error.value)
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
                    new TraceList(List.fromArray(stacktrace))
                  )
                )
              ),
              state
            )
          } else {
            const error_parts = error.message.split("\n")
            const expected = error_parts[1] ? error_parts[1].trim() : "COULD NOT PARSE EXPECTED FROM EXCEPTION"
            const got = error_parts[3] ? error_parts[3].trim() : "COULD NOT PARSE GOT FROM EXCEPTION"
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
                        new Error( new Eq(expected, got, new None()))
                      )
                    ),
                    new TraceList(List.fromArray(stacktrace))
                  )
                )
              ),
              state
            )

          }
        }
      }
      state = eventHandler(new EndTestSuite(test_module), state)
      num_modules++;
    }
    state = eventHandler(new EndTestRun(num_modules), state)
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
  
  function exit(code) {
    if (globalThis.Deno) {
      Deno.exit(code);
    } else {
      process.exit(code);
    }
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