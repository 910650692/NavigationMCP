package com.fy.navi.scene.ui.route;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.databinding.SceneRouteDescendantsViewBinding;
import com.fy.navi.scene.impl.route.SceneRouteDescendantsImpl;
import com.fy.navi.scene.ui.adapter.RouteChildPoiAdapter;
import com.fy.navi.scene.ui.adapter.RouteSecondaryPoiAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;


public class SceneRouteDescendantsView extends BaseSceneView<SceneRouteDescendantsViewBinding, SceneRouteDescendantsImpl> {
    private RouteSecondaryPoiAdapter mRouteSecondaryPoiAdapter;
    private RouteChildPoiAdapter mRouteChildPoiAdapter;
    private OnItemClickListener mItemClickListener;

    public SceneRouteDescendantsView(final Context context) {
        super(context);
    }

    public SceneRouteDescendantsView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneRouteDescendantsView(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneRouteDescendantsViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneRouteDescendantsViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneRouteDescendantsImpl initSceneImpl() {
        return new SceneRouteDescendantsImpl(this);
    }

    @Override
    protected void setInitVariableId() {

    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        mItemClickListener = itemClickListener;
    }

    /**
     * 初始化列表
     * */
    private void setupRecyclerView() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.HORIZONTAL);
        mViewBinding.rvSecondaryPoi.setLayoutManager(layoutManager);
        mRouteSecondaryPoiAdapter = new RouteSecondaryPoiAdapter();
        mViewBinding.rvSecondaryPoi.setAdapter(mRouteSecondaryPoiAdapter);
        mRouteSecondaryPoiAdapter.setItemClickListener(new RouteSecondaryPoiAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final ChildInfo childInfo) {
                final PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPid(childInfo.getPoiId());
                poiInfo.setName(childInfo.getName());
                poiInfo.setAddress(childInfo.getAddress());
                poiInfo.setPoint(new GeoPoint(childInfo.getLocation().getLon(), childInfo.getLocation().getLat()));
                if (mItemClickListener != null) {
                    mItemClickListener.onItemClick(poiInfo);
                }
            }

            @Override
            public void onCancelSelectClick(final PoiInfoEntity poiInfoEntity) {
                if (mItemClickListener != null) {
                    mItemClickListener.onCancelSelectClick(poiInfoEntity);
                }
            }
        });

        mViewBinding.rvSecondaryPoi.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (mItemClickListener != null) {
                    mItemClickListener.OnScrollListener();
                }
            }

            @Override
            public void onScrolled(@NonNull final RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
            }
        });

        final LinearLayoutManager layoutManager2 = new LinearLayoutManager(getContext());
        layoutManager2.setOrientation(LinearLayoutManager.HORIZONTAL);
        mViewBinding.routeDescendantsList.setLayoutManager(layoutManager2);
        mRouteChildPoiAdapter = new RouteChildPoiAdapter();
        mViewBinding.routeDescendantsList.setAdapter(mRouteChildPoiAdapter);
        mRouteChildPoiAdapter.setItemClickListener(new RouteChildPoiAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final ChildInfo childInfo, final PoiInfoEntity poiInfoEntity) {
                if (childInfo.getMGrandChildInfoList() != null && !childInfo.getMGrandChildInfoList().isEmpty()) {
                    mRouteSecondaryPoiAdapter.setChildInfoList(childInfo.getMGrandChildInfoList(), poiInfoEntity);
                } else {
                    List<ChildInfo> grandChildInfoList = new ArrayList<>();
                    grandChildInfoList.add(childInfo);
                    mRouteSecondaryPoiAdapter.setChildInfoList(grandChildInfoList, poiInfoEntity);
                    if (mItemClickListener != null) {
                        mItemClickListener.OnScrollListener();
                    }
                }
            }
        });

        mViewBinding.routeDescendantsList.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(@NonNull final RecyclerView recyclerView, final int newState) {
                super.onScrollStateChanged(recyclerView, newState);
                if (mItemClickListener != null) {
                    mItemClickListener.OnScrollListener();
                }
            }

            @Override
            public void onScrolled(@NonNull final RecyclerView recyclerView, final int dx, final int dy) {
                super.onScrolled(recyclerView, dx, dy);
            }
        });

    }

    /**
     * 设置显示模式
     * @param type type
     * @param poiInfoEntity 当前POI数据
     * */
    public void setUIMode(final int type, final PoiInfoEntity poiInfoEntity) {
        switch (type) {
            //无子孙节点
            case AutoMapConstant.ChildType.DEFAULT:
                mViewBinding.lyDescendantsView.setVisibility(View.GONE);
                break;
            case AutoMapConstant.ChildType.HAS_CHILD_NO_GRAND:
                mViewBinding.lyDescendantsView.setVisibility(View.VISIBLE);
                mViewBinding.sceneRouteDescendantsTopView.setVisibility(View.GONE);
                mRouteSecondaryPoiAdapter.setChildInfoList(poiInfoEntity.getChildInfoList(), poiInfoEntity);
                break;
            case AutoMapConstant.ChildType.HAS_CHILD_HAS_GRAND:
                mViewBinding.lyDescendantsView.setVisibility(View.VISIBLE);
                mViewBinding.sceneRouteDescendantsTopView.setVisibility(View.VISIBLE);
                mViewBinding.routeDescendantsText.setVisibility(View.GONE);
                mViewBinding.routeDescendantsList.setVisibility(View.VISIBLE);
                mRouteChildPoiAdapter.setChildInfoList(poiInfoEntity.getChildInfoList(), poiInfoEntity);
                mRouteChildPoiAdapter.setSelected(0);
                break;
            case AutoMapConstant.ChildType.CHILD_HAS_GRAND:
                mViewBinding.lyDescendantsView.setVisibility(View.VISIBLE);
                mViewBinding.sceneRouteDescendantsTopView.setVisibility(View.VISIBLE);
                mViewBinding.routeDescendantsText.setVisibility(View.VISIBLE);
                mViewBinding.routeDescendantsList.setVisibility(View.GONE);
                mRouteSecondaryPoiAdapter.setChildInfoList(poiInfoEntity.getChildInfoList(), poiInfoEntity);
                break;
            default:
                break;
        }
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param poiInfo 子节点对象
         */
        void onItemClick(PoiInfoEntity poiInfo);

        /***
         * 子节点取消选中
         * @param poiInfoEntity 父节点对象
         */
        void onCancelSelectClick(PoiInfoEntity poiInfoEntity);

        /***
         * 滑动事件
         */
        void OnScrollListener();
    }
}
