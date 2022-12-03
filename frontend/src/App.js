import React, { useEffect, useState } from 'react';
import { setConfig } from 'react-hot-loader'
import { Switch, Route, withRouter } from 'react-router-dom';

import { compose } from 'redux'

import Home from './pages/Home/Home';
import SingleClueSolver from './pages/SingleClueSolver/SingleClueSolver';
import CrosswordSolver from "./pages/CrosswordSolver/CrosswordSolver";

import { TransitionGroup, CSSTransition } from "react-transition-group";

import "./App.scss";
import CrosswordInput from "./pages/CrosswordInput/CrosswordInput";

setConfig({
    reloadHooks: false
})

const pages = [
    { path: '/singleSolver', order: 1 },
    { path: '/', order: 2 },
    { path: '/crosswordInput', order: 3 },
    { path: '/crosswordSolver', order: 4 }
]

const App = ({ location }) => {
    const [pageDirection, setPageDirection] = useState()
    const [currentPath, setCurrentPath] = useState(location.pathname)
    const [currentPathOrder, setCurrentPathOrder] = useState(
        pages.filter(({ path }) => path === location.pathname)[0].order
    )
    const currentKey = location.pathname.split('/')[1] || '/'
    useEffect(() => {
        const newPath = location.pathname
        const newPathOrder = pages.filter(({ path }) => path === newPath)[0].order
        if (newPath !== currentPath) {
            const direction = currentPathOrder < newPathOrder ? 'left' : 'right'
            setCurrentPath(newPath)
            setCurrentPathOrder(newPathOrder)
            setPageDirection(direction)
        }
    }, [setCurrentPath, setCurrentPathOrder, setPageDirection, location, currentPathOrder, currentPath])

    return (
        <TransitionGroup className={`${pageDirection}`}>
            <CSSTransition key={currentKey} timeout={1500} classNames={'route'}>
                <div className="route__container">
                    <Switch location={location}>
                        <Route path="/" exact component={Home} />
                        <Route path="/singleSolver" component={SingleClueSolver} />
                        <Route path="/crosswordInput" component={CrosswordInput} />
                        <Route path="/crosswordSolver" component={CrosswordSolver} />
                    </Switch>
                </div>
            </CSSTransition>
        </TransitionGroup>
    )

}


export default compose(withRouter)(App);
