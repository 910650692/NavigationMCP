package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.ui.navi.SceneNaviNearProvideCharge;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.logicpaket.search.SearchPackage;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description:数据请求
 */
public class SceneNaviNearProvideChargeImpl extends BaseSceneModel<SceneNaviNearProvideCharge> {
    private SearchPackage mSearchPackage;

    public SceneNaviNearProvideChargeImpl(SceneNaviNearProvideCharge screenView) {
        super(screenView);
        mSearchPackage = SearchPackage.getInstance();
    }

    /**
     * 获取充电桩与当前位置的距离
     * @param point
     * @return
     */
    public String getChargeDistance(GeoPoint point) {
        if (point == null) {
            return "";
        }
        if (!ConvertUtils.isNull(mSearchPackage)) {
            return mSearchPackage.calcStraightDistance(point);
        }
        return "";
    }

}
