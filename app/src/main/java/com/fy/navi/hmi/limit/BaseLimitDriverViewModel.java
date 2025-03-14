package com.fy.navi.hmi.limit;

import static com.fy.navi.service.define.map.MapTypeId.MAIN_SCREEN_MAIN_MAP;

import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;

import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;

/**
 * Author: QiuYaWei
 * Date: 2025/2/7
 * Description: [限行控制的基类]
 */
public class BaseLimitDriverViewModel extends BaseViewModel<LimitDriveFragment, LimitDriverModel> {
    private String selectedCityName;

    public String getSelectedCityName() {
        return selectedCityName;
    }

    public void setSelectedCityName(String selectedCityName) {
        this.selectedCityName = selectedCityName;
    }

    public BaseLimitDriverViewModel(@NonNull Application application) {
        super(application);
    }

    @Override
    protected LimitDriverModel initModel() {
        return new LimitDriverModel();
    }

    public void queryLimitPolicyByCityCode(String cityCode) {
        mModel.queryLimitPolicyByCityCode(cityCode);
    }

    public void loadingFail() {
        mView.showLoadingFail();
    }

    public void queryRetry() {
        mModel.queryRetry();
    }

    public void queryLimitResult(RouteRestrictionParam param) {
        mView.showPolicyUI(param);
    }

    // 防止点击穿透
    public Action rootClick = () -> {
    };

    public Action closePage = () -> {
        StackManager.getInstance().getCurrentFragment(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);

        LimitDriverHelper limitDriverHelper = LimitDriverHelper.getInstance();
        if (limitDriverHelper.isNeedClearRestriction()) {
            RoutePackage.getInstance().clearRestrictionView(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        } else if (limitDriverHelper.getRoundParam() != null) {
            RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                    limitDriverHelper.getRoundParam().getGReStrictedAreaResponseParam(), 0);
        }
        MapPackage.getInstance().setMapCenter(MAIN_SCREEN_MAIN_MAP, new GeoPoint(
                PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
    };

    public Action closeCitySelectionPage = () -> {
        StackManager.getInstance().getCurrentFragment(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);
    };

    public Action otherCitySelection = () -> {
        Bundle bundle = new Bundle();
        bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_SELECTION, selectedCityName);
        addFragment(new LimitCitySelectionFragment(), bundle);
    };
}
