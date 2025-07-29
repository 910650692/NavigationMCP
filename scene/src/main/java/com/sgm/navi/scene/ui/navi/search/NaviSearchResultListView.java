package com.sgm.navi.scene.ui.navi.search;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.android.utils.ConvertUtils;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneView;
import com.sgm.navi.scene.api.route.ISceneRouteSearchRefreshItemCallBack;
import com.sgm.navi.scene.databinding.SceneRouteSearchRefreshViewBinding;
import com.sgm.navi.scene.impl.navi.search.NaviSearchResultListViewImpl;
import com.sgm.navi.scene.ui.adapter.RouteSearchRefreshAdapter;
import com.sgm.navi.scene.ui.adapter.navi.NaviSearchResultAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.Hashtable;
import java.util.List;

public class NaviSearchResultListView extends BaseSceneView<SceneRouteSearchRefreshViewBinding,
        NaviSearchResultListViewImpl> {

    public static final String TAG = "NaviSearchResultListView";

    private NaviSearchResultAdapter mAdapter;

    private Hashtable<String, ISceneRouteSearchRefreshItemCallBack>
            mNaviSearchResultItemCallbackHashtable;

    public NaviSearchResultListView(@NonNull Context context) {
        super(context);
    }

    public NaviSearchResultListView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public NaviSearchResultListView(@NonNull Context context, @Nullable AttributeSet attrs,
                                    int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteSearchRefreshViewBinding createViewBinding(LayoutInflater inflater,
                                                                   ViewGroup viewGroup) {
        return SceneRouteSearchRefreshViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected NaviSearchResultListViewImpl initSceneImpl() {
        mNaviSearchResultItemCallbackHashtable = new Hashtable<>();
        return new NaviSearchResultListViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * fragment注册监听
     * @param key 关键字
     * @param callBack 回调
     * */
    public void registerNaviSearchResultObserver(final String key,
                                                 final ISceneRouteSearchRefreshItemCallBack
                                                         callBack) {
        mNaviSearchResultItemCallbackHashtable.put(key, callBack);
    }

    /**
     * 初始化列表
     * */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.routeResult.setLayoutManager(layoutManager);
        mAdapter = new NaviSearchResultAdapter();
        mAdapter.setItemClickListener(new RouteSearchRefreshAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final PoiInfoEntity poiInfoEntity, final int index) {
                for (ISceneRouteSearchRefreshItemCallBack callBack :
                        mNaviSearchResultItemCallbackHashtable.values()) {
                    if (ConvertUtils.isEmpty(callBack)) {
                        continue;
                    }
                    callBack.enterToDetails(poiInfoEntity, index);
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

            }
        });
        mViewBinding.routeResult.setAdapter(mAdapter);
    }

    /**
     * 更新列表
     * @param poiInfoEntities 列表数据
     * */
    public void notifyResultList(final List<PoiInfoEntity> poiInfoEntities) {
        ThreadManager.getInstance().postUi(() -> mAdapter.setPoiInfoEntityList(poiInfoEntities));
    }
}
