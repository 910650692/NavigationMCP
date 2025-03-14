package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRouteSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteResultListViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteResultListImpl;
import com.fy.navi.scene.ui.adapter.RouteResultAdapter;
import com.fy.navi.service.define.route.RouteLineInfo;

import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteResultListView extends BaseSceneView<SceneRouteResultListViewBinding, SceneRouteResultListImpl> {

    private RouteResultAdapter mAdapter;
    private Hashtable<String, ISceneRouteSelectCallBack> sceneRouteSelectCallBackHashtable;

    public SceneRouteResultListView(@NonNull Context context) {
        super(context);
    }

    public SceneRouteResultListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteResultListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteResultListViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneRouteResultListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteResultListImpl initSceneImpl() {
        sceneRouteSelectCallBackHashtable = new Hashtable<>();
        return new SceneRouteResultListImpl(this);
    }

    public void registerRouteSelectObserver(String key, ISceneRouteSelectCallBack callBack) {
        sceneRouteSelectCallBackHashtable.put(key, callBack);
    }



    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    public SceneRouteResultListViewBinding getBinding() {
        return mViewBinding;
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
    }

    private void setupRecyclerView() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.routeResult.setLayoutManager(layoutManager);

        mAdapter = new RouteResultAdapter();
        mAdapter.setItemClickListener((index, isSelectIndex) -> {
            if (!isSelectIndex) {
                mScreenViewModel.selectRoute(index);
            }
            for (ISceneRouteSelectCallBack callBack : sceneRouteSelectCallBackHashtable.values()) {
                if (ConvertUtils.isEmpty(callBack)) continue;
                callBack.onRouteSelect(isSelectIndex, index);
            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
        mViewBinding.routeResult.setOnTouchListener((view, motionEvent) -> {
            for (ISceneRouteSelectCallBack callBack : sceneRouteSelectCallBackHashtable.values()) {
                if (ConvertUtils.isEmpty(callBack)) continue;
                callBack.onListTouch();
            }
            return false;
        });
    }

    public void notifyResultList(List<RouteLineInfo> routeLineInfos) {
        mAdapter.setRouteBeanList(routeLineInfos);
        for (ISceneRouteSelectCallBack callBack : sceneRouteSelectCallBackHashtable.values()) {
            if (ConvertUtils.isEmpty(callBack)) continue;
            callBack.onResultListUpdate();
        }
    }

    public void updateSelectRouteUI(int routeIndex) {
        mAdapter.setSelectIndex(routeIndex);
    }

    public RouteLineInfo getSelectLineInfo() {
        return mAdapter.getSelectLineInfo();
    }

    public int getCurrentIndex() {
        return mAdapter.getCurrentIndex();
    }
}
