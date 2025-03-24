package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;


import com.android.utils.ConvertUtils;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRouteGasStationWeatherServiceSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteStationWeatherServiceBinding;
import com.fy.navi.scene.impl.route.SceneRouteGasStationWeatherServiceImpl;

import java.util.Hashtable;

public class SceneRouteGasStationWeatherServiceView extends BaseSceneView<SceneRouteStationWeatherServiceBinding
        , SceneRouteGasStationWeatherServiceImpl> {

    private Hashtable<String, ISceneRouteGasStationWeatherServiceSelectCallBack> mSceneRouteGasStationWeatherServiceSelectCallBack;

    public SceneRouteGasStationWeatherServiceView(final Context context) {
        super(context);
    }

    public SceneRouteGasStationWeatherServiceView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteGasStationWeatherServiceView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteStationWeatherServiceBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteStationWeatherServiceBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteGasStationWeatherServiceImpl initSceneImpl() {
        mSceneRouteGasStationWeatherServiceSelectCallBack = new Hashtable<>();
        return new SceneRouteGasStationWeatherServiceImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteSelectObserver(final String key, final ISceneRouteGasStationWeatherServiceSelectCallBack callBack) {
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
     * 更新Tab
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
     * 取消Tab的状态
     * */
    public void clearSceneTabUI() {
        mScreenViewModel.setGasStationSelect(false);
        mScreenViewModel.setWeatherSelect(false);
        mScreenViewModel.setServiceSelect(false);
        updateUi();
    }
    /**
     * 回调Tab的点击
     * @param tabIndex tab 索引
     * @param isChceked tab 是否选中
     * */
    public void clickTab(final int tabIndex, final boolean isChceked) {
        for (ISceneRouteGasStationWeatherServiceSelectCallBack callBack : mSceneRouteGasStationWeatherServiceSelectCallBack.values()) {
            if (ConvertUtils.isEmpty(callBack)) {
                continue;
            }
            callBack.onTabListClick(tabIndex, isChceked);
        }
    }
}
