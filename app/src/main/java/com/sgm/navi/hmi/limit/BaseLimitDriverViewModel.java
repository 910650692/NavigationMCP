package com.sgm.navi.hmi.limit;


import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.lifecycle.MutableLiveData;

import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteRestrictionParam;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseViewModel;
import com.sgm.navi.ui.base.StackManager;

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

    public MutableLiveData<Boolean> mTextViewVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mCloseViewVisibility = new MutableLiveData<>(false);
    public MutableLiveData<Boolean> mSearchCityVisibility = new MutableLiveData<>(false);
    public MutableLiveData<String> mTextViewContent = new MutableLiveData<>("");
    public MutableLiveData<String> mEditTextContent = new MutableLiveData<>("");

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
     * 绘制限行区域
     *
     * @param routeRestrictionParam 城市id
     */
    public void drawRestrictionForLimit(final RouteRestrictionParam routeRestrictionParam) {
        mModel.drawRestrictionForLimit(routeRestrictionParam);
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


    private final Action mClosePage = this::finishFragment;

    public Action getClosePage() {
        return mClosePage;
    }

    private final Action mCloseCitySelectionPage = () -> {
        StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);
    };


    private final Action mClearTextContent = () -> {
        mEditTextContent.setValue("");
    };

    public Action getClearTextContent() {
        return mClearTextContent;
    }

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

    @Override
    protected void onBackPressed() {
        finishFragment();
    }

    private void finishFragment() {
        StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name()).closeFragment(true);

        final LimitDriverHelper limitDriverHelper = LimitDriverHelper.getInstance();
        if (limitDriverHelper.isNeedClearRestriction()) {
            RoutePackage.getInstance().clearRestrictionView(MapType.MAIN_SCREEN_MAIN_MAP);
        } else if (limitDriverHelper.getRoundParam() != null) {
            RoutePackage.getInstance().drawRestrictionForLimit(MapType.MAIN_SCREEN_MAIN_MAP,
                    limitDriverHelper.getRoundParam().getMReStrictedAreaResponseParam(), 0);
        }
        MapPackage.getInstance().setMapCenter(MapType.MAIN_SCREEN_MAIN_MAP, new GeoPoint(
                PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
    }
}
