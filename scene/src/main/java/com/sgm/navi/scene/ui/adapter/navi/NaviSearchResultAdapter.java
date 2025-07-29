package com.sgm.navi.scene.ui.adapter.navi;

import android.annotation.SuppressLint;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.RouteSearchRefreshListItemBinding;
import com.sgm.navi.scene.ui.adapter.RouteSearchRefreshAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;

public class NaviSearchResultAdapter extends RecyclerView.Adapter<NaviSearchResultAdapter.Holder>{

    private List<PoiInfoEntity> mPoiInfoEntityList;

    RouteSearchRefreshAdapter.OnItemClickListener itemClickListener;

    public NaviSearchResultAdapter() {
        mPoiInfoEntityList = new ArrayList<>();
    }

    public void setPoiInfoEntityList(List<PoiInfoEntity> poiInfoEntityList) {
        if (null == poiInfoEntityList) {
            return;
        }
        mPoiInfoEntityList.clear();
        mPoiInfoEntityList.addAll(poiInfoEntityList);
        notifyDataSetChanged();
    }

    public void setItemClickListener(RouteSearchRefreshAdapter.
                                             OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        RouteSearchRefreshListItemBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.route_search_refresh_list_item, parent, false);
        return new NaviSearchResultAdapter.Holder(routeItemBinding);
    }

    @SuppressLint("SetTextI18n")
    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.routeSearchRefreshListItemBinding.routeItemServiceNum.setText(
                "" + (position + NumberUtils.NUM_1));
        holder.routeSearchRefreshListItemBinding.routeItemServiceName.setText(
                mPoiInfoEntityList.get(position).getName());
        holder.routeSearchRefreshListItemBinding.routeItemServiceDescription.setText(
                TimeUtils.getInstance().getDistanceString(mPoiInfoEntityList.get(position).
                        getSort_distance()));
        boolean belongRouteParam = RoutePackage.getInstance().isBelongRouteParam(
                MapType.MAIN_SCREEN_MAIN_MAP, mPoiInfoEntityList.get(position));
        holder.routeSearchRefreshListItemBinding.routeItemServiceAddText.setText(
                belongRouteParam ? ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_added) : ResourceUtils.Companion.getInstance().getText(R.string.route_service_list_item_add));
        holder.routeSearchRefreshListItemBinding.routeItemServiceAddImg.setImageDrawable
                (belongRouteParam ? ResourceUtils.Companion.getInstance().
                        getDrawable(R.drawable.img_route_search_added) :
                        ResourceUtils.Companion.getInstance().
                                getDrawable(R.drawable.img_route_search_add));
        holder.routeSearchRefreshListItemBinding.itemRootViewService.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemClick(mPoiInfoEntityList.get(position), position);
            }
        });

        holder.routeSearchRefreshListItemBinding.routeItemServiceAddBg.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItermAddClick(mPoiInfoEntityList.get(position));
            }
        });
    }

    @Override
    public int getItemCount() {
        if (ConvertUtils.isEmpty(mPoiInfoEntityList)) {
            return 0;
        }
        return mPoiInfoEntityList.size();
    }

    public class Holder extends RecyclerView.ViewHolder {
        public RouteSearchRefreshListItemBinding routeSearchRefreshListItemBinding;

        public Holder(RouteSearchRefreshListItemBinding routeSearchRefreshListItemBinding) {
            super(routeSearchRefreshListItemBinding.getRoot());
            this.routeSearchRefreshListItemBinding = routeSearchRefreshListItemBinding;
        }
    }

    public interface OnItemClickListener {
        void onItemClick(PoiInfoEntity poiInfoEntity);
        void onItermAddClick(PoiInfoEntity poiInfoEntity);
    }
}
