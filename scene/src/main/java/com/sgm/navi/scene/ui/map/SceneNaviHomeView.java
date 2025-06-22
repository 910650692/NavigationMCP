package com.sgm.navi.scene.ui.map;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.databinding.SceneNaviHomeViewBinding;
import com.sgm.navi.scene.impl.map.SceneNaviHomeImpl;
import com.sgm.navi.service.define.route.RouteParam;

/***
 * @author yawei
 * @desc 回家场景
 */
public class SceneNaviHomeView extends BaseSceneView<SceneNaviHomeViewBinding, SceneNaviHomeImpl> {
    private static final String TAG = "SceneNaviHomeView";
    private Object mNaviInfo;

    public SceneNaviHomeView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviHomeView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviHomeView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneNaviHomeViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviHomeViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviHomeImpl initSceneImpl() {
        return new SceneNaviHomeImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setISceneCalculate(mScreenViewModel);
    }

    /***
     * TODO 待完善
     * 更新View,这里的对象类型要根据实际需要替换,根据info内容去设置文字
     * @param info
     */
    public void updateView(Object info) {
        this.mNaviInfo = info;
        // mViewBinding.homeSkinTvTitle.setText("");
        // mViewBinding.homeSkinTvSubTitle.setText("");
    }

    @Override
    protected void initObserver() {
        mViewBinding.homeSkinBtnGo.setOnClickListener(v -> {
            // TODO routerParam要根据 mNaviInfo来构建出来
            RouteParam routeParam = null;
            mScreenViewModel.calculateRoad(routeParam);
        });
    }
}
