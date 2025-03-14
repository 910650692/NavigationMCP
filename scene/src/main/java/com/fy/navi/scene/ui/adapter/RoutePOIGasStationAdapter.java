package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RoutePoiGasStationItemBinding;
import com.fy.navi.scene.databinding.RoutePoiIconItemBinding;
import com.fy.navi.service.define.route.RouteLineInfo;

import java.util.ArrayList;
import java.util.List;

public class RoutePOIGasStationAdapter extends RecyclerView.Adapter<RoutePOIGasStationAdapter.Holder> {
    private List<String> mRouteBeanList;
    OnItemClickListener itemClickListener;

    public RoutePOIGasStationAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    public void setRouteBeanList(List<String> routeBeanList) {
        if (null == routeBeanList) {
            return;
        }

        mRouteBeanList.clear();
        mRouteBeanList.addAll(routeBeanList);
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        RoutePoiGasStationItemBinding routeItemBinding =
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
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.routePoiGasStationItemBinding.poiServiceAreaGasOilType.setText(mRouteBeanList.get(position));
    }

    public class Holder extends RecyclerView.ViewHolder {
        public RoutePoiGasStationItemBinding routePoiGasStationItemBinding;

        public Holder(RoutePoiGasStationItemBinding routePoiGasStationItemBinding) {
            super(routePoiGasStationItemBinding.getRoot());
            this.routePoiGasStationItemBinding = routePoiGasStationItemBinding;
            routePoiGasStationItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int index, boolean isSelectIndex);
    }
}