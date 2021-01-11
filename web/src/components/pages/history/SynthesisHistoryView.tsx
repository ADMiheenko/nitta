import React, { useEffect, useState, useContext } from "react";
import ReactTable, { Column } from "react-table";
import { haskellApiService } from "../../../services/HaskellApiService";
import { NodeView, NId, IBindDecisionView } from "../../../gen/types";
import { AxiosResponse, AxiosError } from "axios";
import { AppContext, IAppContext } from "../../app/AppContext";

type Row = { original: Node; index: number };
type Node = NodeView<string, string, string, string>;

export interface ISynthesisHistoryViewProps {
  reverse: boolean;
}

export const SynthesisHistoryView: React.FC<ISynthesisHistoryViewProps> = (props) => {
  const appContext = useContext(AppContext) as IAppContext;
  const [synthesisHistory, setHistory] = useState<Node[]>();
  const style = {
    fontWeight: 600,
    width: "100%",
  };

  useEffect(() => {
    haskellApiService
      .getRootPath(appContext.selectedNodeId)
      .then((response: AxiosResponse<Node[]>) => {
        let result = response.data;
        if (props.reverse) {
          result = result.reverse();
        }
        setHistory(result);
      })
      .catch((err: AxiosError) => console.log(err));
  }, [appContext.selectedNodeId, props.reverse]);

  function Table(props: { name: string; columns: Column[]; history: Node[] }) {
    if (props.history.length === 0)
      return (
        <small>
          <pre style={style}>{props.name}: NOTHING</pre>
        </small>
      );
    return (
      <small style={style}>
        <pre className="squeze h5">{props.name}</pre>
        <ReactTable
          defaultPageSize={props.history.length}
          minRows={props.history.length}
          showPagination={false}
          columns={props.columns}
          data={props.history}
        />
        <br />
      </small>
    );
  }

  function stepNumber(row: Row) {
    return props.reverse ? synthesisHistory!.length - row.index : row.index + 1;
  }

  function stepColumn(onUpdateNid: (sid: NId) => void) {
    return {
      Header: "step",
      maxWidth: 40,
      Cell: (row: Row) => {
        let sid = row.original.sid;
        if (sid === appContext.selectedNodeId) return <>{stepNumber(row)}</>;
        return (
          <button className="btn-link bg-transparent p-0 border-0" onClick={() => onUpdateNid(sid)}>
            {stepNumber(row)}
          </button>
        );
      },
    };
  }

  function textColumn(
    columnName: string,
    f: (n: Node) => string | React.ReactElement,
    maxWidth?: number,
    minWidth?: number
  ) {
    return {
      Header: columnName,
      style: style,
      maxWidth: maxWidth,
      minWidth: minWidth,
      Cell: (row: { original: Node }) => f(row.original),
    };
  }

  if (synthesisHistory == null) return <pre>LOADING...</pre>;

  return (
    <>
      <Table
        name="History"
        history={synthesisHistory}
        columns={[
          stepColumn(appContext.selectNode),
          textColumn("decision type", (n: Node) => n.decision.tag, 100),
          textColumn("description", (n: Node) => {
            if (n.sid === "-") return <>INITIAL STATE</>;
            let decision = n.decision.tag;
            return (
              <>
                {decision === "BindDecisionView" &&
                  (n.decision as IBindDecisionView).pu + " <- " + (n.decision as IBindDecisionView).function.fvFun}
                {decision === "DataflowDecisionView" && JSON.stringify(n.decision)}
                {decision === "RefactorDecisionView" && JSON.stringify(n.decision)}
              </>
            );
          }),
        ]}
      />
    </>
  );
};
