package com.sgm.navi.hmi.route;

import android.app.Application;

public class RouteViewModel extends BaseRouteViewModel {
    public RouteViewModel(final Application application) {
        super(application);
        setMsgDialogTop((int) application.getResources()
                .getDimension(com.sgm.navi.service.R.dimen.dp_53));
        setMsgDialogLeft((int) application.getResources()
                .getDimension(com.sgm.navi.service.R.dimen.dp_579));
    }

    @Override
    public boolean isNewAlterCharge() {
        return true;
    }
}
