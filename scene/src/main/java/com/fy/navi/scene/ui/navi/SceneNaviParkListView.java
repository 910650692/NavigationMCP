package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_CONTROL;
import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_PARALLEL;
import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_PARK_LIST;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviParkListViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviParkListImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.adapter.NaviParkListAdapter;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 列表scene 停车场列表
 */
public class SceneNaviParkListView extends NaviSceneBase<SceneNaviParkListViewBinding, SceneNaviParkListImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private NaviParkListAdapter mNaviParkListAdapter;

    public SceneNaviParkListView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviParkListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviParkListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_PARK_LIST;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_PARK_LIST, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_PARK_LIST, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_PARK_LIST, true);
        }
    }

    @Override
    protected SceneNaviParkListViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviParkListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviParkListImpl initSceneImpl() {
        return new SceneNaviParkListImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviParkList(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        Logger.d(TAG, "SceneNaviListView initObserver");
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.srvAddVia.setLayoutManager(layoutManager);

        mNaviParkListAdapter = new NaviParkListAdapter();
        mViewBinding.srvAddVia.setAdapter(mNaviParkListAdapter);
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }

    public void showNaviParkList(List<NaviParkingEntity> list, boolean isCheck, int select) {
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        mNaviParkListAdapter.setOnItemClickListener(mScreenViewModel);
        mScreenViewModel.showParkingMark(select);
        if (isCheck) {
            for (int i = 0; i < list.size(); i++) {
                NaviParkingEntity naviParkingEntity = list.get(i);
                if (naviParkingEntity.isEndPoi) {
                    notifyList(List.of(naviParkingEntity), select);
                    return;
                }
            }
        }
        Logger.i(TAG, "SceneNaviParkListImpl list：" + list.size());
        notifyList(list, select);
    }

    public void notifyList(List<NaviParkingEntity> list, int select) {
        mNaviParkListAdapter.notifyList(list, select);
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.checkParking(naviEtaInfo);
        }
    }
}
