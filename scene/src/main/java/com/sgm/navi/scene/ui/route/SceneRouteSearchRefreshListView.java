package com.sgm.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.api.route.ISceneRouteSearchRefreshItemCallBack;
import com.sgm.navi.scene.databinding.SceneRouteSearchRefreshViewBinding;
import com.sgm.navi.scene.impl.route.SceneRouteSeachRefreshListImpl;
import com.sgm.navi.scene.ui.adapter.RouteSearchRefreshAdapter;
import com.sgm.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.Hashtable;
import java.util.List;


public class SceneRouteSearchRefreshListView extends BaseSceneView<SceneRouteSearchRefreshViewBinding, SceneRouteSeachRefreshListImpl> {

    private RouteSearchRefreshAdapter mAdapter;
    private Hashtable<String, ISceneRouteSearchRefreshItemCallBack> mSearchRefreshItemCallBackHashtable;

    public SceneRouteSearchRefreshListView(final Context context) {
        super(context);
    }

    public SceneRouteSearchRefreshListView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteSearchRefreshListView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteSearchRefreshViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteSearchRefreshViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteSeachRefreshListImpl initSceneImpl() {
        mSearchRefreshItemCallBackHashtable = new Hashtable<>();
        return new SceneRouteSeachRefreshListImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteSearchRefreshObserver(final String key, final ISceneRouteSearchRefreshItemCallBack callBack) {
        mSearchRefreshItemCallBackHashtable.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    public SceneRouteSearchRefreshViewBinding getBinding() {
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
        mAdapter = new RouteSearchRefreshAdapter();
        mAdapter.setItemClickListener(new RouteSearchRefreshAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchRefreshItemCallBack callBack : mSearchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) {
                        continue;
                    }
                    callBack.enterToDetails(poiInfoEntity);
                }
            }

            @Override
            public void onItermAddClick(final PoiInfoEntity poiInfoEntity) {
                mScreenViewModel.onItermAddClick(poiInfoEntity);
            }

            /***
             * 点击删除按钮
             * @param poiInfoEntity 点信息
             */
            @Override
            public void onItermRemoveClick(final PoiInfoEntity poiInfoEntity) {
                mScreenViewModel.onItermRemoveClick(poiInfoEntity);
            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
    }
    /**
     * 更新列表
     * @param poiInfoEntities 列表数据
     * */
    public void notifyResultList(final List<RouteRestAreaDetailsInfo> poiInfoEntities) {
        ThreadManager.getInstance().postUi(() -> mAdapter.setRouteBeanList(poiInfoEntities));
    }
}
