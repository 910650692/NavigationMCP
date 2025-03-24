package com.fy.navi.hmi.limit;


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
 * @author  QiuYaWei
 * @version  \$Revision.1.0\$
 * Date: 2025/2/7
 * Description: [限行控制的基类]
 */
public class BaseLimitDriverViewModel extends BaseViewModel<LimitDriveFragment, LimitDriverModel> {
    private String mSelectedCityName;

    public String getSelectedCityName() {
        return mSelectedCityName;
    }

    public void setSelectedCityName(final String selectedCityName) {
        this.mSelectedCityName = selectedCityName;
    }

    public BaseLimitDriverViewModel(final @NonNull Application application) {
        super(application);
    }

    @Override
    protected LimitDriverModel initModel() {
        return new LimitDriverModel();
    }

    /**
     * 获取限行政策
     *
     * @param cityCode 城市id
     */
    public void queryLimitPolicyByCityCode(final String cityCode) {
        mModel.queryLimitPolicyByCityCode(cityCode);
    }

    /**
     * 显示加载失败
     *
     */
    public void loadingFail() {
        mView.showLoadingFail();
    }

    /**
     * 发起代码重试
     *
     */
    public void queryRetry() {
        mModel.queryRetry();
    }

    /**
     * 显示限行政策UI
     * @param param 城市id
     */
    public void showPolicyUI(final RouteRestrictionParam param) {
        mView.showPolicyUI(param);
    }

    // 防止点击穿透
    private final Action mRootClick = () -> {
    };

    public Action getRootClick() {
        return mRootClick;
    }

    private final Action mClosePage = () -> {
        StackManager.getInstance().getCurrentFragment(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);

        final LimitDriverHelper limitDriverHelper = LimitDriverHelper.getInstance();
        if (limitDriverHelper.isNeedClearRestriction()) {
            RoutePackage.getInstance().clearRestrictionView(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        } else if (limitDriverHelper.getRoundParam() != null) {
            RoutePackage.getInstance().drawRestrictionForLimit(MapTypeId.MAIN_SCREEN_MAIN_MAP,
                    limitDriverHelper.getRoundParam().getMReStrictedAreaResponseParam(), 0);
        }
        MapPackage.getInstance().setMapCenter(MapTypeId.MAIN_SCREEN_MAIN_MAP, new GeoPoint(
                PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
    };

    public Action getClosePage() {
        return mClosePage;
    }

    private final Action mCloseCitySelectionPage = () -> {
        StackManager.getInstance().getCurrentFragment(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);
    };

    public Action getCloseCitySelectionPage() {
        return mCloseCitySelectionPage;
    }

    private final Action mOtherCitySelection = () -> {
        final Bundle bundle = new Bundle();
        bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_CITY_SELECTION, mSelectedCityName);
        addFragment(new LimitCitySelectionFragment(), bundle);
    };

    public Action getOtherCitySelection() {
        return mOtherCitySelection;
    }
}
