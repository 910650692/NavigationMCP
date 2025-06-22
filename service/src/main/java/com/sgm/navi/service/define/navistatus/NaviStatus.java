package com.sgm.navi.service.define.navistatus;

import androidx.annotation.StringDef;


public class NaviStatus {
    @StringDef({
            NaviStatusType.NO_STATUS,
            NaviStatusType.SELECT_ROUTE,
            NaviStatusType.ROUTING,
            NaviStatusType.NAVING,
            NaviStatusType.CRUISE,
            NaviStatusType.LIGHT_NAVING})
    public @interface NaviStatusType {
        String NO_STATUS = "NO_STATUS";
        String SELECT_ROUTE = "SELECT_ROUTE";
        String ROUTING = "ROUTING";
        String NAVING = "NAVING";
        String CRUISE = "CRUISE";
        String LIGHT_NAVING = "LIGHT_NAVING";
    }
}
