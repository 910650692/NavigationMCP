package com.fy.navi.scene.ui.route;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.route.ISceneRouteGasStationChargeSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteStationChargeServiceBinding;
import com.fy.navi.scene.impl.route.SceneRouteGasStationChargeServiceImpl;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.Hashtable;

public class SceneRouteGasStationChargeServiceView
        extends BaseSceneView<SceneRouteStationChargeServiceBinding, SceneRouteGasStationChargeServiceImpl> {

    private Hashtable<String, ISceneRouteGasStationChargeSelectCallBack> mSceneRouteGasStationWeatherServiceSelectCallBack;

    public SceneRouteGasStationChargeServiceView(final Context context) {
        super(context);
    }

    public SceneRouteGasStationChargeServiceView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteGasStationChargeServiceView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteStationChargeServiceBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteStationChargeServiceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteGasStationChargeServiceImpl initSceneImpl() {
        mSceneRouteGasStationWeatherServiceSelectCallBack = new Hashtable<>();
        return new SceneRouteGasStationChargeServiceImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteSelectObserver(final String key, final ISceneRouteGasStationChargeSelectCallBack callBack) {
        mSceneRouteGasStationWeatherServiceSelectCallBack.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }
    /**
     * 更新UI
     * */
    public void updateUi() {
        if (ConvertUtils.isEmpty(mViewBinding) || ConvertUtils.isEmpty(mScreenViewModel)) {
            return;
        }
        mViewBinding.routeRightTabListTvGasStation.setSelected(mScreenViewModel.isGasStationSelect());
        mViewBinding.routeRightTabListIvGasStation.setSelected(mScreenViewModel.isGasStationSelect());
        mViewBinding.routeRightTabListTvWeather.setSelected(mScreenViewModel.isWeatherSelect());
        mViewBinding.routeRightTabListIvWeather.setSelected(mScreenViewModel.isWeatherSelect());
        mViewBinding.routeRightTabListTvService.setSelected(mScreenViewModel.isServiceSelect());
        mViewBinding.routeRightTabListIvService.setSelected(mScreenViewModel.isServiceSelect());
    }
    /**
     * 点击Tab
     * @param tabIndex 索引
     * */
    public void clickTab(final int tabIndex) {
        for (ISceneRouteGasStationChargeSelectCallBack callBack : mSceneRouteGasStationWeatherServiceSelectCallBack.values()) {
            if (ConvertUtils.isEmpty(callBack)) {
                continue;
            }
            callBack.onTabListGasChargeClick(tabIndex);
        }
    }
    /**
     * 高亮沿途
     * */
    public void highlightAlongTab() {
        if (mScreenViewModel == null) {
            Logger.d("mScreenViewModel is null ");
            return;
        }
        mScreenViewModel.setGasStationSelect(true);
        mScreenViewModel.setWeatherSelect(false);
        mScreenViewModel.setServiceSelect(false);
        updateUi();
    }

    /**
     * 设置沿途搜是否为充电站
     * @param isCharge 是否是充电站
     * */
    @SuppressLint("UseCompatLoadingForDrawables")
    public void setSearchCharge(boolean isCharge) {
        Logger.d(MapDefaultFinalTag.ROUTE_HMI_TAG,"isCharge: ",isCharge);
        final Drawable chargeDrawable = getContext().getDrawable(R.drawable.selector_route_tab_list_charge_gas_end);
        final Drawable pointDrawable = getContext().getDrawable(R.drawable.selector_route_tab_list_point_end);
        mViewBinding.routeRightTabListIvWeather.setImageDrawable(isCharge ?  chargeDrawable : pointDrawable);
    }
}
