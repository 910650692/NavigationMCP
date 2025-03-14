package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshItemCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSelectCallBack;
import com.fy.navi.scene.databinding.SceneRouteResultListViewBinding;
import com.fy.navi.scene.databinding.SceneRouteSearchRefreshViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteResultListImpl;
import com.fy.navi.scene.impl.route.SceneRouteSeachRefreshListImpl;
import com.fy.navi.scene.ui.adapter.RouteResultAdapter;
import com.fy.navi.scene.ui.adapter.RouteSearchRefreshAdapter;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.route.RouteRestAreaInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteSearchRefreshListView extends BaseSceneView<SceneRouteSearchRefreshViewBinding, SceneRouteSeachRefreshListImpl> {

    private RouteSearchRefreshAdapter mAdapter;
    private Hashtable<String, ISceneRouteSearchRefreshItemCallBack> searchRefreshItemCallBackHashtable;

    public SceneRouteSearchRefreshListView(@NonNull Context context) {
        super(context);
    }

    public SceneRouteSearchRefreshListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteSearchRefreshListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteSearchRefreshViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneRouteSearchRefreshViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteSeachRefreshListImpl initSceneImpl() {
        searchRefreshItemCallBackHashtable = new Hashtable<>();
        return new SceneRouteSeachRefreshListImpl(this);
    }

    public void registerRouteSearchRefreshObserver(String key, ISceneRouteSearchRefreshItemCallBack callBack) {
        searchRefreshItemCallBackHashtable.put(key, callBack);
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

    private void setupRecyclerView() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.routeResult.setLayoutManager(layoutManager);
        mAdapter = new RouteSearchRefreshAdapter();
        mAdapter.setItemClickListener(new RouteSearchRefreshAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchRefreshItemCallBack callBack : searchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) continue;
                    callBack.enterToDetails(poiInfoEntity);
                }
            }

            @Override
            public void onItermAddClick(PoiInfoEntity poiInfoEntity) {
                mScreenViewModel.onItermAddClick(poiInfoEntity);
            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
    }

    public void notifyResultList(List<PoiInfoEntity> poiInfoEntities) {
        ThreadManager.getInstance().postUi(() -> mAdapter.setRouteBeanList(poiInfoEntities));
    }
}
