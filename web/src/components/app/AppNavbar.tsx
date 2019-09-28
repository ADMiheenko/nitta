import * as React from "react";
import { Navbar, Nav } from "react-bootstrap";
import { NavLink, Link } from "react-router-dom";

import "./AppNavbar.scss";

import AppContext from "./AppContext";

export interface IAppNavbarProps {}

export interface IAppNavbarState {}

export default class AppNavbar extends React.Component<IAppNavbarProps, IAppNavbarState> {
  static contextType = AppContext;
  context!: React.ContextType<typeof AppContext>;

  constructor(props: IAppNavbarProps) {
    super(props);

    this.state = {};
  }

  public render() {
    return (
      <Navbar bg="dark" variant="dark" expand="md" sticky="top" className="appNavbar">
        <Navbar.Brand className="mr-md-5 ml-4" as={Link} to="/">
          <img alt="Project Logo" src="/itmo_logo.png" height="30" className="mr-2 d-inline-block align-center"></img>
          NITTA Web UI
        </Navbar.Brand>
        <Navbar.Collapse id="navbar-nav">
          <Nav className="mr-auto">
            {/* "as" property can help us to integrate React Router's NavLink */}
            <Nav.Link as={NavLink} exact to="/">
              Home
            </Nav.Link>
          </Nav>
        </Navbar.Collapse>
        <Navbar.Text className="mr-5">Selected Node ID: {this.context.selectedNodeId || "None"}</Navbar.Text>
        <Navbar.Toggle aria-controls="navbar-nav" />
      </Navbar>
    );
  }
}
