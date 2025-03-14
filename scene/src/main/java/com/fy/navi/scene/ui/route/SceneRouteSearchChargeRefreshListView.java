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
import com.fy.navi.scene.api.route.ISceneRouteSearchChargeRefreshItemCallBack;
import com.fy.navi.scene.api.route.ISceneRouteSearchRefreshItemCallBack;
import com.fy.navi.scene.databinding.SceneRouteSearchChargeRefreshViewBinding;
import com.fy.navi.scene.databinding.SceneRouteSearchRefreshViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteSeachChargeRefreshListImpl;
import com.fy.navi.scene.impl.route.SceneRouteSeachRefreshListImpl;
import com.fy.navi.scene.ui.adapter.RouteSearchChargeRefreshAdapter;
import com.fy.navi.scene.ui.adapter.RouteSearchRefreshAdapter;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.Hashtable;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class SceneRouteSearchChargeRefreshListView extends BaseSceneView<SceneRouteSearchChargeRefreshViewBinding, SceneRouteSeachChargeRefreshListImpl> {

    private RouteSearchChargeRefreshAdapter mAdapter;
    private Hashtable<String, ISceneRouteSearchChargeRefreshItemCallBack> searchRefreshItemCallBackHashtable;
    private List<PoiInfoEntity> localPoiInfoEntities;

    public SceneRouteSearchChargeRefreshListView(@NonNull Context context) {
        super(context);
    }

    public SceneRouteSearchChargeRefreshListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteSearchChargeRefreshListView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteSearchChargeRefreshViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneRouteSearchChargeRefreshViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteSeachChargeRefreshListImpl initSceneImpl() {
        searchRefreshItemCallBackHashtable = new Hashtable<>();
        return new SceneRouteSeachChargeRefreshListImpl(this);
    }

    public void registerRouteSearchRefreshObserver(String key, ISceneRouteSearchChargeRefreshItemCallBack callBack) {
        searchRefreshItemCallBackHashtable.put(key, callBack);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setScene(mScreenViewModel);
    }

    public SceneRouteSearchChargeRefreshViewBinding getBinding() {
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
        mAdapter = new RouteSearchChargeRefreshAdapter();
        mAdapter.setItemClickListener(new RouteSearchChargeRefreshAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchChargeRefreshItemCallBack callBack : searchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) continue;
                    callBack.enterToChargeDetails(poiInfoEntity);
                }
            }

            @Override
            public void onItermRemoveClick(PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchChargeRefreshItemCallBack callBack : searchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) continue;
                    callBack.onGasChargeRemoveClick(poiInfoEntity);
                }
            }

            @Override
            public void onItermAddClick(PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchChargeRefreshItemCallBack callBack : searchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) continue;
                    callBack.onGasChargeAddClick(poiInfoEntity);
                }
            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
    }

    public void notifyResultList(List<PoiInfoEntity> poiInfoEntities, List<RouteParam> loaclSaveEntity, int searchType) {
        ThreadManager.getInstance().postUi(() -> {
            mAdapter.setRouteBeanList(poiInfoEntities, loaclSaveEntity, searchType);
        });
        localPoiInfoEntities = poiInfoEntities;
    }

    public void updateChargeList(List<RouteParam> loaclSaveEntity, int searchType) {
        mAdapter.setRouteBeanList(localPoiInfoEntities, loaclSaveEntity, searchType);
    }
}
