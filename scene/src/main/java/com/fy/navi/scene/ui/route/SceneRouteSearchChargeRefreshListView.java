package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import androidx.recyclerview.widget.LinearLayoutManager;
import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.api.route.ISceneRouteSearchChargeRefreshItemCallBack;
import com.fy.navi.scene.databinding.SceneRouteSearchChargeRefreshViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteSeachChargeRefreshListImpl;
import com.fy.navi.scene.ui.adapter.RouteSearchChargeRefreshAdapter;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.Hashtable;
import java.util.List;


public class SceneRouteSearchChargeRefreshListView
        extends BaseSceneView<SceneRouteSearchChargeRefreshViewBinding, SceneRouteSeachChargeRefreshListImpl> {

    private RouteSearchChargeRefreshAdapter mAdapter;
    private Hashtable<String, ISceneRouteSearchChargeRefreshItemCallBack> mSearchRefreshItemCallBackHashtable;
    private List<PoiInfoEntity> mLocalPoiInfoEntities;

    public SceneRouteSearchChargeRefreshListView(final Context context) {
        super(context);
    }

    public SceneRouteSearchChargeRefreshListView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteSearchChargeRefreshListView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteSearchChargeRefreshViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteSearchChargeRefreshViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteSeachChargeRefreshListImpl initSceneImpl() {
        mSearchRefreshItemCallBackHashtable = new Hashtable<>();
        return new SceneRouteSeachChargeRefreshListImpl(this);
    }
    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerRouteSearchRefreshObserver(final String key, final ISceneRouteSearchChargeRefreshItemCallBack callBack) {
        mSearchRefreshItemCallBackHashtable.put(key, callBack);
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
    /**
     * 初始化列表
     * */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.routeResult.setLayoutManager(layoutManager);
        mAdapter = new RouteSearchChargeRefreshAdapter();
        mAdapter.setmItemClickListener(new RouteSearchChargeRefreshAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchChargeRefreshItemCallBack callBack : mSearchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) {
                        continue;
                    }
                    callBack.enterToChargeDetails(poiInfoEntity);
                }
            }

            @Override
            public void onItermRemoveClick(final PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchChargeRefreshItemCallBack callBack : mSearchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) {
                        continue;
                    }
                    callBack.onGasChargeRemoveClick(poiInfoEntity);
                }
            }

            @Override
            public void onItermAddClick(final PoiInfoEntity poiInfoEntity) {
                for (ISceneRouteSearchChargeRefreshItemCallBack callBack : mSearchRefreshItemCallBackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) {
                        continue;
                    }
                    callBack.onGasChargeAddClick(poiInfoEntity);
                }
            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
    }
    /***
     * 展示充电站列表
     * @param poiInfoEntities 搜索数据
     * @param loaclSaveEntity 本地已添加数据
     * @param searchType 搜索方式 0-沿途搜索
     * @param type 列表类别 0:充电站 1：加油站
     */
    public void notifyResultList(final List<PoiInfoEntity> poiInfoEntities,
                                 final List<RouteParam> loaclSaveEntity, final int searchType, final int type) {
        ThreadManager.getInstance().postUi(() -> {
            mAdapter.setRouteBeanList(poiInfoEntities, loaclSaveEntity, searchType, type);
        });
        mLocalPoiInfoEntities = poiInfoEntities;
    }
    /***
     * 更新充电本地数据
     * @param loaclSaveEntity 本地数据
     * @param searchType 搜索方式
     */
    public void updateChargeList(final List<RouteParam> loaclSaveEntity, final int searchType) {
        mAdapter.updateRouteBeanList(mLocalPoiInfoEntities, loaclSaveEntity, searchType);
    }
}
