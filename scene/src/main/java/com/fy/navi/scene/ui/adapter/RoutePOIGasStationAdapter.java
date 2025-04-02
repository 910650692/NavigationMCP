package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RoutePoiGasStationItemBinding;

import java.util.ArrayList;
import java.util.List;

public class RoutePOIGasStationAdapter extends RecyclerView.Adapter<RoutePOIGasStationAdapter.Holder> {
    private List<String> mRouteBeanList;
    private OnItemClickListener mItemClickListener;

    public RoutePOIGasStationAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    /***
     * 设置数据
     * @param routeBeanList 数据列表
     */
    public void setRouteBeanList(final List<String> routeBeanList) {
        if (null == routeBeanList) {
            return;
        }

        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        notifyDataSetChanged();
    }

    /***
     * 设置监听
     * @param itemClickListener 点击监听
     */
    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final RoutePoiGasStationItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_poi_gas_station_item, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mRouteBeanList == null) {
            return 0;
        }
        return mRouteBeanList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull final Holder holder, final int position) {
        holder.mRoutePoiGasStationItemBinding.poiServiceAreaGasOilType.setText(mRouteBeanList.get(position));
    }

    public class Holder extends RecyclerView.ViewHolder {
        private RoutePoiGasStationItemBinding mRoutePoiGasStationItemBinding;

        public Holder(final RoutePoiGasStationItemBinding routePoiGasStationItemBinding) {
            super(routePoiGasStationItemBinding.getRoot());
            this.mRoutePoiGasStationItemBinding = routePoiGasStationItemBinding;
            routePoiGasStationItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        /***
         * 点击监听
         * @param index 下标
         * @param isSelectIndex 选中下标
         */
        void onItemClick(int index, boolean isSelectIndex);
    }
}