package com.fy.navi.scene.ui.adapter;


import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteReplaceSupplementItemBinding;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;


public class RouteReplaceSupplementAdapter extends RecyclerView.Adapter<RouteReplaceSupplementAdapter.Holder>{
    private List<RouteAlterChargeStationInfo> mData = new ArrayList<>();

    public RouteReplaceSupplementAdapter() {
        mData = new ArrayList<>();
    }

    /***
     * 设置可替换补能点显示
     * @param routeAlterChargeStationInfos 子节点列表
     */
    public void setRouteSupplementInfos(final ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos) {
        if (null == routeAlterChargeStationInfos) {
            return;
        }
        mData.clear();
        mData.addAll(routeAlterChargeStationInfos);
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RouteReplaceSupplementItemBinding routeReplaceSupplementItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_replace_supplement_item, parent, false);
        return new RouteReplaceSupplementAdapter.Holder(routeReplaceSupplementItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {


    }

    @Override
    public int getItemCount() {
        return mData.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RouteReplaceSupplementItemBinding mRouteReplaceSupplementItemBinding;

        public Holder(@NonNull final RouteReplaceSupplementItemBinding routeReplaceSupplementItemBinding) {
            super(routeReplaceSupplementItemBinding.getRoot());
            this.mRouteReplaceSupplementItemBinding = routeReplaceSupplementItemBinding;
            mRouteReplaceSupplementItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /***
         * 子节点点击事件
         * @param childInfo 子节点对象
         * @param poiInfoEntity 当前位置
         */
        void onItemClick(ChildInfo childInfo, PoiInfoEntity poiInfoEntity);

    }
}
