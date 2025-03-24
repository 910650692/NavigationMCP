package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRouteSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteResultListViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteResultListImpl;
import com.fy.navi.scene.ui.adapter.RouteResultAdapter;
import com.fy.navi.service.define.route.RouteLineInfo;

import java.util.Hashtable;
import java.util.List;

public class SceneRouteResultListView extends BaseSceneView<SceneRouteResultListViewBinding, SceneRouteResultListImpl> {

    private RouteResultAdapter mAdapter;
    private Hashtable<String, ISceneRouteSelectCallBack> mSceneRouteSelectCallBackHashtable;

    public SceneRouteResultListView(final Context context) {
        super(context);
    }

    public SceneRouteResultListView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteResultListView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteResultListViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteResultListViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteResultListImpl initSceneImpl() {
        mSceneRouteSelectCallBackHashtable = new Hashtable<>();
        return new SceneRouteResultListImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteSelectObserver(final String key, final ISceneRouteSelectCallBack callBack) {
        mSceneRouteSelectCallBackHashtable.put(key, callBack);
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
    /**
     * 初始化列表
     * */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.routeResult.setLayoutManager(layoutManager);

        mAdapter = new RouteResultAdapter();
        mAdapter.setmItemClickListener((index, isSelectIndex) -> {
            if (!isSelectIndex) {
                mScreenViewModel.selectRoute(index);
            }
            for (ISceneRouteSelectCallBack callBack : mSceneRouteSelectCallBackHashtable.values()) {
                if (ConvertUtils.isEmpty(callBack)) {
                    continue;
                }
                callBack.onRouteSelect(isSelectIndex, index);
            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
        mViewBinding.routeResult.setOnTouchListener((view, motionEvent) -> {
            for (ISceneRouteSelectCallBack callBack : mSceneRouteSelectCallBackHashtable.values()) {
                if (ConvertUtils.isEmpty(callBack)) {
                    continue;
                }
                callBack.onListTouch();
            }
            return false;
        });
    }
    /**
     * 刷新列表
     * @param routeLineInfos 列表数据
     * */
    public void notifyResultList(final List<RouteLineInfo> routeLineInfos) {
        mAdapter.setRouteBeanList(routeLineInfos);
        for (ISceneRouteSelectCallBack callBack : mSceneRouteSelectCallBackHashtable.values()) {
            if (ConvertUtils.isEmpty(callBack)) {
                continue;
            }
            callBack.onResultListUpdate();
        }
    }
    /**
     * 更新列表选中item
     * @param routeIndex 路线索引
     * */
    public void updateSelectRouteUI(final int routeIndex) {
        mAdapter.setSelectIndex(routeIndex);
    }
}
