import React from "react";
import { unmountComponentAtNode } from "react-dom";
import renderer from 'react-test-renderer';
import { BrowserRouter as Router } from "react-router-dom";
import SingleSolverResult from "./SingleSolverResult";


let container = null;
beforeEach(() => {
  // setup a DOM element as a render target
  container = document.createElement("div");
  document.body.appendChild(container);
});

afterEach(() => {
  // cleanup on exiting
  unmountComponentAtNode(container);
  container.remove();
  container = null;
});

it('renders correctly', () => {
  const tree = renderer
    .create(<Router><SingleSolverResult /></Router>)
    .toJSON();
  expect(tree).toMatchSnapshot();
});