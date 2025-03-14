package com.fy.navi.service.logicpaket.l2;

import com.fy.navi.service.define.navi.L2NaviBean;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public interface L2InfoCallback {

    void onNaviStatus(L2NaviBean l2NaviBean);

    void onSelectRouteIndex(L2NaviBean l2NaviBean);
}
