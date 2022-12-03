import React from "react";
import { unmountComponentAtNode } from "react-dom";
import renderer from 'react-test-renderer';
import Timeline from "./Timeline";
import { BrowserRouter as Router } from "react-router-dom";


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
    .create(<Router><Timeline /></Router>)
    .toJSON();
  expect(tree).toMatchSnapshot();
});