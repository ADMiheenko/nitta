import React, { FC, useContext } from "react";
import "react-table/react-table.css";

import { AppContext, IAppContext } from "app/AppContext";
import { IntermediateView } from "components/IntermediateView";
import { MicroarchitectureView } from "components/MicroarchitectureView";
import { TreeInfoView } from "components/TreeInfoView";

export interface INodeScreenProps {}

export const NodeScreen: FC<INodeScreenProps> = (props) => {
  const { selectedSID } = useContext(AppContext) as IAppContext;

  if (!selectedSID) return <pre> synthesis is not selected </pre>;

  return (
    <div className="m-3">
      <h3>sid:</h3>
      <pre>{selectedSID}</pre>

      <h3>Tree info:</h3>
      <TreeInfoView />

      <h3 className="mt-3">Dataflow graph:</h3>
      <IntermediateView />

      <MicroarchitectureView />
    </div>
  );
};
