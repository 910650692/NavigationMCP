package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteLineInfoResultItemBinding;
import com.fy.navi.scene.databinding.RoutePoiIconItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.search.ServiceAreaInfo;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class RoutePOIIconAdapter extends RecyclerView.Adapter<RoutePOIIconAdapter.Holder> {
    private List<ServiceAreaInfo.ServiceAreaChild> mRouteBeanList;
    OnItemClickListener itemClickListener;

    public RoutePOIIconAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    public void setRouteBeanList(List<ServiceAreaInfo.ServiceAreaChild> routeBeanList) {
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
        RoutePoiIconItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_poi_icon_item, parent, false);
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
        switch (mRouteBeanList.get(position).getTypeCode()) {
            case "010200":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_gas);
                break;
            case "200300":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_wc);
                break;
            case "060400":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_shopping);
                break;
            case "150904":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_parking);
                break;
            case "011100":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_charging);
                break;
            case "010000":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_car_service);
                break;
            case "050100":
                holder.routePoiIconItemBinding.poiServiceAreaImgLighting.setImageResource(R.drawable.img_route_poi_details_food);
                break;
        }
    }

    public class Holder extends RecyclerView.ViewHolder {
        public RoutePoiIconItemBinding routePoiIconItemBinding;

        public Holder(RoutePoiIconItemBinding routePoiIconItemBinding) {
            super(routePoiIconItemBinding.getRoot());
            this.routePoiIconItemBinding = routePoiIconItemBinding;
            routePoiIconItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int index, boolean isSelectIndex);
    }
}