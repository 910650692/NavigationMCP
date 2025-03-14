package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_VIA_ARRIVED_POP;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviViaArriveViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviViaArriveViewImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.List;

public class SceneNaviViaArriveView extends NaviSceneBase<SceneNaviViaArriveViewBinding, SceneNaviViaArriveViewImpl> {
    public static final String TAG = "SceneNaviViaArriveView";

    private long currentViaIndex = -1;

    public SceneNaviViaArriveView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviViaArriveView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaArriveView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public void show() {
        super.show();
        Logger.i(TAG, "show");
        // 提前点击显示，第一个途经点没有经过所以默认的viaIndex为-1
        List<RouteParam> allPoiParamList = RoutePackage.getInstance().
                getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() > 2) {
            if (currentViaIndex == -1) {
                // 默认显示第一个
                mViewBinding.stvViaInfo.setText(String.format(getResources().
                        getString(R.string.is_arrived), allPoiParamList.get(1).getName()));
            } else {
                // 因为是提前显示这边得加二
                mViewBinding.stvViaInfo.setText(String.format(getResources().
                        getString(R.string.is_arrived), allPoiParamList.
                        get((int) currentViaIndex + 2).getName()));
            }
        }
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_VIA_ARRIVED_POP, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_VIA_ARRIVED_POP, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_VIA_ARRIVED_POP, false);
        }
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_VIA_ARRIVED_POP;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_VIA_ARRIVED_POP, this);

    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        mScreenViewModel.addISceneCallback(sceneCallback);
    }

    @Override
    protected SceneNaviViaArriveViewBinding createViewBinding(
            LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviViaArriveViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviViaArriveViewImpl initSceneImpl() {
        return new SceneNaviViaArriveViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviArrive(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    public void onUpdateViaPass(long viaIndex) {
        Logger.i(TAG, "onUpdateViaPass viaIndex = " + viaIndex);
        currentViaIndex = viaIndex;
    }

    public void startNavigation() {
        Logger.i(TAG, "startNavigation");
        currentViaIndex = -1;
    }
}
