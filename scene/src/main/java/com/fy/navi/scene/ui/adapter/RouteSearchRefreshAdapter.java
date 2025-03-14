package com.fy.navi.scene.ui.adapter;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.RouteSearchRefreshListItemBinding;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteRestAreaDetailsInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;

public class RouteSearchRefreshAdapter extends RecyclerView.Adapter<RouteSearchRefreshAdapter.Holder> {
    private List<PoiInfoEntity> mRouteBeanList;
    OnItemClickListener itemClickListener;
    public RouteSearchRefreshAdapter() {
        mRouteBeanList = new ArrayList<>();
    }

    public void setRouteBeanList(List<PoiInfoEntity> routeBeanList) {
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
        RouteSearchRefreshListItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_search_refresh_list_item, parent, false);
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
        holder.routeSearchRefreshListItemBinding.routeItemServiceNum.setText("" + (position + NumberUtils.NUM_1));
        holder.routeSearchRefreshListItemBinding.routeItemServiceName.setText(mRouteBeanList.get(position).getName());
        holder.routeSearchRefreshListItemBinding.routeItemServiceDescription.setText(mRouteBeanList.get(position).getDistance() + " | " + mRouteBeanList.get(position).getAddress());
        boolean belongRouteParam = RoutePackage.getInstance().isBelongRouteParam(MapTypeId.MAIN_SCREEN_MAIN_MAP, mRouteBeanList.get(position));
        holder.routeSearchRefreshListItemBinding.routeItemServiceAddText.setText(belongRouteParam ? ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_added) : ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_add));
        holder.routeSearchRefreshListItemBinding.routeItemServiceAddImg.setImageDrawable(belongRouteParam ? ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_added) : ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_route_search_add));
        holder.routeSearchRefreshListItemBinding.itemRootViewService.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemClick(mRouteBeanList.get(position));
            }
        });

        holder.routeSearchRefreshListItemBinding.routeItemServiceAddBg.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItermAddClick(mRouteBeanList.get(position));
            }
        });
    }

    public class Holder extends RecyclerView.ViewHolder {
        public RouteSearchRefreshListItemBinding routeSearchRefreshListItemBinding;

        public Holder(RouteSearchRefreshListItemBinding routeSearchRefreshListItemBinding) {
            super(routeSearchRefreshListItemBinding.getRoot());
            this.routeSearchRefreshListItemBinding = routeSearchRefreshListItemBinding;
            routeSearchRefreshListItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(PoiInfoEntity poiInfoEntity);
        void onItermAddClick(PoiInfoEntity poiInfoEntity);
    }
}