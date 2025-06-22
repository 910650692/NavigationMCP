package com.sgm.navi.scene.ui.navi;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviViaArriveViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviViaArriveViewImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.util.List;

public class SceneNaviViaArriveView extends NaviSceneBase<SceneNaviViaArriveViewBinding, SceneNaviViaArriveViewImpl> {
    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_ARRIVE;

    public SceneNaviViaArriveView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviViaArriveView(@NonNull final Context context,
                                  @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaArriveView(@NonNull final Context context,
                                  @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public void show() {
        super.show();
        Logger.i(TAG, "show");
        // 提前点击显示，第一个途经点没有经过所以默认的viaIndex为-1
        final List<RouteParam> allPoiParamList = RoutePackage.getInstance().
                getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() > 2) {
            // 显示第一个途经点
            mViewBinding.stvViaInfo.setText(String.format(getResources().
                            getString(R.string.is_arrived),
                    allPoiParamList.get(1).getName()));
        }
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_VIA_ARRIVED_POP;
    }

    @Override
    protected SceneNaviViaArriveViewBinding createViewBinding(
            final LayoutInflater inflater, final ViewGroup viewGroup) {
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

}
