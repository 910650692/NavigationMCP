package com.sgm.navi.hmi.route;

import android.app.Application;

public class RouteViewModel extends BaseRouteViewModel {
    public RouteViewModel(final Application application) {
        super(application);
        setMsgDialogTop(100);
        setMsgDialogLeft(700);
    }

    @Override
    public boolean isNDCar() {
        return true;
    }
}
